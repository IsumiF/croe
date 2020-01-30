module CROE.Backend.Clock.Base
  ( runClock
  ) where

import           Data.Time
import           Polysemy

import           CROE.Backend.Clock.Class

runClock :: Member (Embed IO) r
         => Sem (Clock : r) a
         -> Sem r a
runClock = interpret $ \case
    CurrentTimeUTC -> embed getCurrentTime
