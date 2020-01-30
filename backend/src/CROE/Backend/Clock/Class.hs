module CROE.Backend.Clock.Class
  ( Clock(..)
  , currentTimeUTC
  ) where

import           Data.Time
import           Polysemy

data Clock (m :: * -> *) r where
  CurrentTimeUTC :: Clock m UTCTime

makeSem ''Clock
