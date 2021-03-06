{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- | Instances requires for consensus/ledger integration
module Ouroboros.Consensus.Byron.Ledger.Ledger (
    ByronTransition(..)
    -- * Ledger integration
  , initByronLedgerState
  , byronEraParams
  , byronEraParamsNeverHardForks
    -- * Serialisation
  , encodeByronAnnTip
  , decodeByronAnnTip
  , encodeByronExtLedgerState
  , encodeByronHeaderState
  , encodeByronLedgerState
  , decodeByronLedgerState
  , encodeByronQuery
  , decodeByronQuery
  , encodeByronResult
  , decodeByronResult
    -- * Type family instances
  , Ticked(..)
  , LedgerState(..)
  , Query(..)
    -- * Auxiliary
  , validationErrorImpossible
  ) where

import           Codec.CBOR.Decoding (Decoder)
import qualified Codec.CBOR.Decoding as CBOR
import           Codec.CBOR.Encoding (Encoding)
import qualified Codec.CBOR.Encoding as CBOR
import           Codec.Serialise (decode, encode)
import           Control.Monad.Except
import           Data.ByteString (ByteString)
import           Data.Kind (Type)
import           GHC.Generics (Generic)

import           Cardano.Binary (encodeListLen, enforceSize, fromCBOR, toCBOR)
import           Cardano.Prelude (NoUnexpectedThunks)

import qualified Cardano.Chain.Block as CC
import qualified Cardano.Chain.Byron.API as CC
import qualified Cardano.Chain.Genesis as Gen
import qualified Cardano.Chain.Update as Update
import qualified Cardano.Chain.Update.Validation.Interface as UPI
import qualified Cardano.Chain.UTxO as CC
import qualified Cardano.Chain.ValidationMode as CC

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Forecast
import           Ouroboros.Consensus.HardFork.Abstract
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.CommonProtocolParams
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Protocol.PBFT
import           Ouroboros.Consensus.Util (ShowProxy (..))

import           Ouroboros.Consensus.Byron.Ledger.Block
import           Ouroboros.Consensus.Byron.Ledger.Conversions
import           Ouroboros.Consensus.Byron.Ledger.HeaderValidation ()
import           Ouroboros.Consensus.Byron.Ledger.PBFT
import           Ouroboros.Consensus.Byron.Ledger.Serialisation

{-------------------------------------------------------------------------------
  LedgerState
-------------------------------------------------------------------------------}

data instance LedgerState ByronBlock = ByronLedgerState {
      byronLedgerTipBlockNo :: !(WithOrigin BlockNo)
    , byronLedgerState      :: !CC.ChainValidationState
    , byronLedgerTransition :: !ByronTransition
    }
  deriving (Eq, Show, Generic, NoUnexpectedThunks)

-- | Transition from Byron to Shelley
--
-- Placeholder type until we properly address #2455
data ByronTransition =
    -- | Transition point not yet known
    ByronTransitionUnknown
  deriving (Eq, Show, Generic, NoUnexpectedThunks)

instance UpdateLedger ByronBlock

type instance LedgerCfg (LedgerState ByronBlock) = Gen.Config

initByronLedgerState :: Gen.Config
                     -> Maybe CC.UTxO -- ^ Optionally override UTxO
                     -> LedgerState ByronBlock
initByronLedgerState genesis mUtxo = ByronLedgerState {
      byronLedgerState      = override mUtxo initState
    , byronLedgerTipBlockNo = Origin
    , byronLedgerTransition = ByronTransitionUnknown
    }
  where
    initState :: CC.ChainValidationState
    initState = case runExcept $ CC.initialChainValidationState genesis of
      Right st -> st
      Left e   -> error $
        "could not create initial ChainValidationState: " <> show e

    override :: Maybe CC.UTxO
             -> CC.ChainValidationState -> CC.ChainValidationState
    override Nothing     st = st
    override (Just utxo) st = st { CC.cvsUtxo = utxo }

{-------------------------------------------------------------------------------
  GetTip
-------------------------------------------------------------------------------}

instance GetTip (LedgerState ByronBlock) where
  getTip = castPoint . getByronTip . byronLedgerState

instance GetTip (Ticked (LedgerState ByronBlock)) where
  getTip = castPoint . getByronTip . tickedByronLedgerState

getByronTip :: CC.ChainValidationState -> Point ByronBlock
getByronTip state =
    case CC.cvsPreviousHash state of
      -- In this case there are no blocks in the ledger state. The genesis
      -- block does not occupy a slot, so its point is Origin.
      Left _genHash -> GenesisPoint
      Right hdrHash -> BlockPoint slot (ByronHash hdrHash)
        where
          slot = fromByronSlotNo (CC.cvsLastSlot state)

{-------------------------------------------------------------------------------
  Ticked ledger state
-------------------------------------------------------------------------------}

-- | The ticked Byron ledger state
newtype instance Ticked (LedgerState ByronBlock) = TickedByronLedgerState {
      tickedByronLedgerState :: CC.ChainValidationState
    }
  deriving (Generic, NoUnexpectedThunks)

instance IsLedger (LedgerState ByronBlock) where
  type LedgerErr (LedgerState ByronBlock) = CC.ChainValidationError

  applyChainTick cfg slotNo ByronLedgerState{..} =
      TickedByronLedgerState {
          tickedByronLedgerState =
            CC.applyChainTick cfg (toByronSlotNo slotNo) byronLedgerState
        }

{-------------------------------------------------------------------------------
  Supporting the various consensus interfaces
-------------------------------------------------------------------------------}

instance ApplyBlock (LedgerState ByronBlock) ByronBlock where
  applyLedgerBlock = applyByronBlock validationMode
    where
      validationMode = CC.fromBlockValidationMode CC.BlockValidation

  reapplyLedgerBlock cfg blk st =
      validationErrorImpossible $
        applyByronBlock validationMode cfg blk st
    where
      validationMode = CC.fromBlockValidationMode CC.NoBlockValidation

data instance Query ByronBlock :: Type -> Type where
  GetUpdateInterfaceState :: Query ByronBlock UPI.State

instance QueryLedger ByronBlock where
  answerQuery _cfg GetUpdateInterfaceState ledgerState =
    CC.cvsUpdateState (byronLedgerState ledgerState)

instance SameDepIndex (Query ByronBlock) where
  sameDepIndex GetUpdateInterfaceState GetUpdateInterfaceState = Just Refl

deriving instance Eq (Query ByronBlock result)
deriving instance Show (Query ByronBlock result)

instance ShowQuery (Query ByronBlock) where
  showResult GetUpdateInterfaceState = show

instance ShowProxy (Query ByronBlock) where

instance CommonProtocolParams ByronBlock where
  maxHeaderSize = fromIntegral . Update.ppMaxHeaderSize . getProtocolParameters
  maxTxSize     = fromIntegral . Update.ppMaxTxSize     . getProtocolParameters

-- | Return the protocol parameters adopted by the given ledger.
getProtocolParameters :: LedgerState ByronBlock -> Update.ProtocolParameters
getProtocolParameters =
      CC.adoptedProtocolParameters
    . CC.cvsUpdateState
    . byronLedgerState

instance LedgerSupportsProtocol ByronBlock where
  protocolLedgerView _cfg =
        toTickedPBftLedgerView
      . CC.getDelegationMap
      . tickedByronLedgerState

  -- Create a forecast of the delegation state
  --
  -- We can return forecasts for slots in the @[NOW .. NOW+2k)@ window, where
  -- @NOW@ is the slot number of the last block applied to the ledger.
  --
  -- These forecasts will be used to validate future headers, i.e., to check
  -- whether they have been created by the right delegates.
  --
  -- We cannot look more than @2k@ slots ahead, because there might be
  -- delegation state changes present in the blocks between the last block
  -- applied to the ledger and the header to validate that can kick in after
  -- @2k@ slots.
  --
  -- To create a forecast, take the delegation state from the given ledger
  -- state, and apply the updates that should be applied by the given slot.
  ledgerViewForecastAt cfg (ByronLedgerState _tipBlkNo st _) = Forecast at $ \for ->
      toTickedPBftLedgerView <$> if
        | for == lastSlot ->
          return $ CC.getDelegationMap st
        | for < maxFor ->
          return $ CC.previewDelegationMap (toByronSlotNo for) st
        | otherwise ->
          throwError $ OutsideForecastRange {
              outsideForecastAt     = at
            , outsideForecastMaxFor = maxFor
            , outsideForecastFor    = for
            }
    where
      SecurityParam k = genesisSecurityParam cfg
      lastSlot        = fromByronSlotNo $ CC.cvsLastSlot st
      at              = NotOrigin lastSlot

      -- The upper bound is exclusive
      maxFor :: SlotNo
      maxFor = case at of
          Origin      -> SlotNo $ 2 * k
          NotOrigin s -> SlotNo $ unSlotNo s + 1 + (2 * k)

-- | To be used for a Byron-to-X (where X is typically Shelley) chain.
byronEraParams :: HardFork.SafeBeforeEpoch -> Gen.Config -> HardFork.EraParams
byronEraParams safeBeforeEpoch genesis = HardFork.EraParams {
      eraEpochSize  = fromByronEpochSlots $ Gen.configEpochSlots genesis
    , eraSlotLength = fromByronSlotLength $ genesisSlotLength genesis
    , eraSafeZone   = HardFork.StandardSafeZone (2 * k) safeBeforeEpoch
    }
  where
    SecurityParam k = genesisSecurityParam genesis

-- | Separate variant of 'byronEraParams' to be used for a Byron-only chain.
byronEraParamsNeverHardForks :: Gen.Config -> HardFork.EraParams
byronEraParamsNeverHardForks genesis = HardFork.EraParams {
      eraEpochSize  = fromByronEpochSlots $ Gen.configEpochSlots genesis
    , eraSlotLength = fromByronSlotLength $ genesisSlotLength genesis
    , eraSafeZone   = HardFork.UnsafeIndefiniteSafeZone
    }

instance HasHardForkHistory ByronBlock where
  type HardForkIndices ByronBlock = '[ByronBlock]
  hardForkSummary = neverForksHardForkSummary byronEraParamsNeverHardForks

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

-- | Mark computation as validation error free
--
-- Given a 'BlockValidationMode' of 'NoBlockValidation', a call to
-- 'applyByronBlock' shouldn't fail since the ledger layer won't be performing
-- any block validation checks. However, because 'applyByronBlock' can fail in
-- the event it is given a 'BlockValidationMode' of 'BlockValidation', it still
-- /looks/ like it can fail (since its type doesn't change based on the
-- 'ValidationMode') and we must still treat it as such.
validationErrorImpossible :: forall err a. Except err a -> a
validationErrorImpossible = cantBeError . runExcept
  where
    cantBeError :: Either err a -> a
    cantBeError (Left  _) = error "validationErrorImpossible: unexpected error"
    cantBeError (Right a) = a

{-------------------------------------------------------------------------------
  Applying a block

  Most of the work here is done by the ledger layer. We just need to pass
  the right arguments, and maintain the snapshots.
-------------------------------------------------------------------------------}

applyByronBlock :: CC.ValidationMode
                -> LedgerConfig ByronBlock
                -> ByronBlock
                -> TickedLedgerState ByronBlock
                -> Except (LedgerError ByronBlock) (LedgerState ByronBlock)
applyByronBlock validationMode
                cfg
                blk@(ByronBlock raw _ (ByronHash blkHash))
                ls =
    case raw of
      CC.ABOBBlock    raw' -> applyABlock validationMode cfg raw' blkHash blkNo ls
      CC.ABOBBoundary raw' -> applyABoundaryBlock        cfg raw'         blkNo ls
  where
    blkNo :: BlockNo
    blkNo = blockNo blk

applyABlock :: CC.ValidationMode
            -> Gen.Config
            -> CC.ABlock ByteString
            -> CC.HeaderHash
            -> BlockNo
            -> Ticked (LedgerState (ByronBlock))
            -> Except (LedgerError ByronBlock) (LedgerState ByronBlock)
applyABlock validationMode cfg blk blkHash blkNo TickedByronLedgerState{..} = do
    st' <- CC.validateBlock cfg validationMode blk blkHash tickedByronLedgerState
    return ByronLedgerState {
          byronLedgerTipBlockNo = NotOrigin blkNo
        , byronLedgerState      = st'
        , byronLedgerTransition = ByronTransitionUnknown -- TODO (#2455)
        }

-- | Apply boundary block
--
-- Since boundary blocks don't modify the delegation state, they also don't
-- modify the delegation history.
applyABoundaryBlock :: Gen.Config
                    -> CC.ABoundaryBlock ByteString
                    -> BlockNo
                    -> Ticked (LedgerState ByronBlock)
                    -> Except (LedgerError ByronBlock) (LedgerState ByronBlock)
applyABoundaryBlock cfg blk blkNo TickedByronLedgerState{..} = do
    st' <- CC.validateBoundary cfg blk tickedByronLedgerState
    return ByronLedgerState {
        byronLedgerTipBlockNo = NotOrigin blkNo
      , byronLedgerState      = st'
      , byronLedgerTransition = ByronTransitionUnknown -- TODO (#2455)
      }

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

encodeByronAnnTip :: AnnTip ByronBlock -> Encoding
encodeByronAnnTip = encodeAnnTipIsEBB encodeByronHeaderHash

decodeByronAnnTip :: Decoder s (AnnTip ByronBlock)
decodeByronAnnTip = decodeAnnTipIsEBB decodeByronHeaderHash

encodeByronExtLedgerState :: ExtLedgerState ByronBlock -> Encoding
encodeByronExtLedgerState = encodeExtLedgerState
    encodeByronLedgerState
    encodeByronChainDepState
    encodeByronAnnTip

encodeByronHeaderState :: HeaderState ByronBlock -> Encoding
encodeByronHeaderState = encodeHeaderState
    encodeByronChainDepState
    encodeByronAnnTip

encodeByronTransition :: ByronTransition -> Encoding
encodeByronTransition ByronTransitionUnknown = mconcat [
      CBOR.encodeWord8 0
    ]

decodeByronTransition :: Decoder s ByronTransition
decodeByronTransition = do
    tag <- CBOR.decodeWord8
    case tag of
      0 -> return $ ByronTransitionUnknown
      _ -> fail $ "decodeByronTransition: invalid tag " <> show tag

encodeByronLedgerState :: LedgerState ByronBlock -> Encoding
encodeByronLedgerState ByronLedgerState{..} = mconcat [
      encodeListLen 3
    , encode byronLedgerTipBlockNo
    , encode byronLedgerState
    , encodeByronTransition byronLedgerTransition
    ]

decodeByronLedgerState :: Decoder s (LedgerState ByronBlock)
decodeByronLedgerState = do
    enforceSize "ByronLedgerState" 3
    ByronLedgerState
      <$> decode
      <*> decode
      <*> decodeByronTransition

encodeByronQuery :: Query ByronBlock result -> Encoding
encodeByronQuery query = case query of
    GetUpdateInterfaceState -> CBOR.encodeWord8 0

decodeByronQuery :: Decoder s (SomeBlock Query ByronBlock)
decodeByronQuery = do
    tag <- CBOR.decodeWord8
    case tag of
      0 -> return $ SomeBlock GetUpdateInterfaceState
      _ -> fail $ "decodeByronQuery: invalid tag " <> show tag

encodeByronResult :: Query ByronBlock result -> result -> Encoding
encodeByronResult query = case query of
    GetUpdateInterfaceState -> toCBOR

decodeByronResult :: Query ByronBlock result
                  -> forall s. Decoder s result
decodeByronResult query = case query of
    GetUpdateInterfaceState -> fromCBOR
