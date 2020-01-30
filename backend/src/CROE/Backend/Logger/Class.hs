module CROE.Backend.Logger.Class
  ( Logger(..)
  , printLog
  , LogLevel(..)
  ) where

import           Polysemy
import           System.Log.FastLogger

data LogLevel = LevelDebug
              | LevelInfo
              | LevelWarn
              | LevelError
                deriving (Show, Eq, Ord)

data Logger (m :: * -> *) a where
  PrintLog :: ToLogStr str => LogLevel -> str -> Logger m ()

makeSem ''Logger
