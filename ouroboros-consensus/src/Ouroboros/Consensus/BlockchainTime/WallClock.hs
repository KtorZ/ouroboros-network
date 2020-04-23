{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Supporting definitions for BlockchanTime instances that use the wallclock
module Ouroboros.Consensus.BlockchainTime.WallClock (
    -- * System start
    SystemStart(..)
    -- * Slot length
  , SlotLength(..)
    -- ** Conversions
  , slotLengthFromSec
  , slotLengthToSec
  , slotLengthFromMillisec
  , slotLengthToMillisec
    -- * Tracing
  , TraceBlockchainTimeEvent(..)
    -- * Exceptions
  , SystemClockMovedBackException(..)
    -- * Support for defining 'BlockchainTime' instances
  , waitUntilSystemStart
  ) where

import           Control.Exception (Exception)
import           Control.Monad
import           Control.Tracer
import           Data.Fixed
import           Data.Time (NominalDiffTime, UTCTime, diffUTCTime)
import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks, UseIsNormalForm (..))
import           Cardano.Slotting.Slot

import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer (MonadDelay (..))

import           Ouroboros.Consensus.Util.Time

{-------------------------------------------------------------------------------
  System start
-------------------------------------------------------------------------------}

-- | System start
--
-- Slots are counted from the system start.
newtype SystemStart = SystemStart { getSystemStart :: UTCTime }
  deriving (Eq, Show)
  deriving NoUnexpectedThunks via UseIsNormalForm SystemStart

{-------------------------------------------------------------------------------
  SlotLength
-------------------------------------------------------------------------------}

-- | Slot length
newtype SlotLength = SlotLength { getSlotLength :: NominalDiffTime }
  deriving (Show, Eq, Generic, NoUnexpectedThunks)

slotLengthFromSec :: Integer -> SlotLength
slotLengthFromSec = slotLengthFromMillisec . (* 1000)

slotLengthToSec :: SlotLength -> Integer
slotLengthToSec = (`div` 1000) . slotLengthToMillisec

slotLengthFromMillisec :: Integer -> SlotLength
slotLengthFromMillisec = SlotLength . conv
  where
    -- Explicit type annotation here means that /if/ we change the precision,
    -- we are forced to reconsider this code.
    conv :: Integer -> NominalDiffTime
    conv = (realToFrac :: Pico -> NominalDiffTime)
         . (/ 1000)
         . (fromInteger :: Integer -> Pico)

slotLengthToMillisec :: SlotLength -> Integer
slotLengthToMillisec = conv . getSlotLength
  where
    -- Explicit type annotation here means that /if/ we change the precision,
    -- we are forced to reconsider this code.
    conv :: NominalDiffTime -> Integer
    conv = truncate
         . (* 1000)
         . (realToFrac :: NominalDiffTime -> Pico)

{-------------------------------------------------------------------------------
  Tracing
-------------------------------------------------------------------------------}

-- | Time related tracing
data TraceBlockchainTimeEvent =
    -- | The start time of the blockchain time is in the future
    --
    -- We have to block (for 'NominalDiffTime') until that time comes.
    TraceStartTimeInTheFuture SystemStart NominalDiffTime

    -- | Current slot is not yet known
    --
    -- This happens when the tip of our current chain is so far in the past that
    -- we cannot translate the current wallclock to a slot number, typically
    -- during syncing. Until the current slot number is known, we cannot
    -- produce blocks. Seeing this message during syncing therefore is
    -- normal and to be expected.
    --
    -- We record the current time (the time we tried to translate to a 'SlotNo')
    -- as well as the maximum 'UTCTime' that we /can/ currently translate to a
    -- 'SlotNo'. This distance should  decrease rapidly with consecutive
    -- 'TraceCurrentSlotUnknown' messages.
  | TraceCurrentSlotUnknown UTCTime UTCTime
  deriving (Show)

{-------------------------------------------------------------------------------
  Exceptions
-------------------------------------------------------------------------------}

data SystemClockMovedBackException =
    -- | The system clock got moved back so far that the slot number decreased
    --
    -- We record the time at which we discovered the clock change, the slot
    -- number before the clock change, and the slot number after the change.
    SystemClockMovedBack UTCTime SlotNo SlotNo
  deriving (Show)

instance Exception SystemClockMovedBackException

{-------------------------------------------------------------------------------
  Support for defining 'BlockchainTime' instances
-------------------------------------------------------------------------------}

-- | Wait until system start if necessary
waitUntilSystemStart :: (MonadTime m, MonadDelay m)
                     => Tracer m TraceBlockchainTimeEvent
                     -> SystemStart
                     -> m ()
waitUntilSystemStart tracer start = do
    now <- getCurrentTime
    when (getSystemStart start > now) $ do
      let delay = getSystemStart start `diffUTCTime` now
      traceWith tracer $ TraceStartTimeInTheFuture start delay
      threadDelay (nominalDelay delay)
