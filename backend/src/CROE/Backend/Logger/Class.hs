module CROE.Backend.Logger.Class
  ( Logger(..)
  , printLog
  , printLogt
  , LogLevel(..)
  ) where

import           Data.Text             (Text)
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

printLogt :: Member Logger r => LogLevel -> Text -> Sem r ()
printLogt = printLog
