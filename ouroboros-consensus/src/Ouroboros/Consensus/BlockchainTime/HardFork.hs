{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Consensus.BlockchainTime.HardFork (
    hardForkBlockchainTime
  ) where

import           Control.Monad
import           Control.Tracer
import           Data.Time (NominalDiffTime)
import           Data.Void

import           Ouroboros.Consensus.BlockchainTime.API
import           Ouroboros.Consensus.BlockchainTime.WallClock
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.HardFork.Abstract
import qualified Ouroboros.Consensus.HardFork.History as HF
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry
import           Ouroboros.Consensus.Util.Time

-- | 'BlockchainTime' instance with support for the hard fork history
hardForkBlockchainTime :: forall m blk.
                          ( IOLike m
                          , HasHardForkHistory blk
                          , UpdateLedger blk
                          )
                       => ResourceRegistry m
                       -> Tracer m TraceBlockchainTimeEvent
                       -> SystemStart
                       -> TopLevelConfig blk
                       -> STM m (LedgerState blk)
                       -> m (BlockchainTime m)
hardForkBlockchainTime registry
                       tracer
                       start
                       TopLevelConfig{..}
                       getLedgerState = do
    run <- HF.runWithCachedSummary (summarize <$> getLedgerState)
    waitUntilSystemStart tracer start

    (_now, firstSlot, firstDelay) <- getCurrentSlot' tracer run
    slotVar <- newTVarM firstSlot
    void $ forkLinkedThread registry "hardForkBlockchainTime" $
             loop run slotVar firstSlot firstDelay

    return $ BlockchainTime {
        getCurrentSlot = readTVar slotVar
      }
  where
    summarize :: LedgerState blk -> HF.Summary (HardForkIndices blk)
    summarize st = HF.summarize
                     start
                     (ledgerTipSlot st)
                     (hardForkShape       configBlock)
                     (hardForkTransitions configLedger st)

    loop :: HF.RunWithCachedSummary xs m
         -> StrictTVar m CurrentSlot
         -> CurrentSlot     -- Previous slot
         -> NominalDiffTime -- Time to wait until next slot
         -> m Void
    loop run slotVar = go
      where
        go :: CurrentSlot -> NominalDiffTime -> m Void
        go prevSlot delay = do
           threadDelay (nominalDelay delay)
           (now, newSlot, newDelay) <- getCurrentSlot' tracer run
           checkValidTransition now (prevSlot, newSlot)
           atomically $ writeTVar slotVar newSlot
           go newSlot newDelay

    checkValidTransition :: UTCTime -> (CurrentSlot, CurrentSlot) -> m ()
    checkValidTransition now = \case
        (CurrentSlotUnknown, _) ->
          -- Unknown-to-unknown and unknown-to-known transition both okay
          return ()
        (CurrentSlot _, CurrentSlotUnknown) ->
          error "hardForkBlockchainTime: ledger rolled back more than k"
        (CurrentSlot m, CurrentSlot n)
          | m <  n    -> return () -- The normal case
          | m == n    -> return () -- We allow time to stand still
          | otherwise -> throwM $ SystemClockMovedBack now m n

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

getCurrentSlot' :: forall m xs. IOLike m
                => Tracer m TraceBlockchainTimeEvent
                -> HF.RunWithCachedSummary xs m
                -> m (UTCTime, CurrentSlot, NominalDiffTime)
getCurrentSlot' tracer run = do
    now   <- getCurrentTime
    mSlot <- atomically $ HF.cachedRunQuery run $ HF.wallclockToSlot now
    case mSlot of
      Left (HF.PastHorizon summary _) -> do
        let (_lo, hi) = HF.summaryBounds summary
            horizon   = HF.boundTime hi
        -- If the 'horizon' is very far away, the current tip is very far away
        -- from the wallclock. However, that probably does not mean we have to
        -- wait 'distance' time: we are probably just syncing, and so the tip of
        -- the ledger will rapidly move forward. So at most 'distance' could be
        -- used as a heuristic for how long to wait. For now we just trace it.
        --
        -- Instead, we just return a fixed delay of 60 seconds. There is a
        -- trade-off between trying to often, incurring computational overhead,
        -- and missing the opportunity to produce a block. With a 60 second
        -- delay, computational overhead should be minimal, and the number of
        -- slots we might miss is minimal. We anyway can't guarantee the speed
        -- of syncing, so delaying it by a further 60 seconds does not change
        -- anything fundamentally.
        traceWith tracer $ TraceCurrentSlotUnknown now horizon
        return (now, CurrentSlotUnknown, 60)
      Right (slot, inSlot) ->
        return (now, CurrentSlot slot, inSlot)
