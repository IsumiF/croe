{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module CROE.Backend.Logger
  ( Env
  , HasEnv(..)
  , withEnv
  , runLoggingT
  , Config
  ) where

import           Control.Monad.IO.Class
import           Control.Monad.Logger   hiding (runLoggingT)
import           Data.Aeson
import           Data.Maybe
import           Data.Text              (Text)

newtype Env = Env Config

newtype Config = Config
  { _config_level :: LogLevel
  }

instance FromJSON Config where
  parseJSON = withObject "Logger.Config" $ \o ->
    Config . readLogLevel <$> o .: "level"

readLogLevel :: Text -> LogLevel
readLogLevel str = fromMaybe (LevelOther str) (lookup str levelMap)
  where
    levelMap =
      [ ("Debug", LevelDebug)
      , ("Info", LevelInfo)
      , ("Warn", LevelWarn)
      , ("Error", LevelError)
      ]

class HasEnv r where
  getEnv :: r -> Env

instance HasEnv Env where
  getEnv = id

withEnv :: MonadIO m
        => Config -> (Env -> m a) -> m a
withEnv c f = f (Env c)

runLoggingT :: (MonadIO m, HasEnv r)
            => r -> LoggingT m a -> m a
runLoggingT r m =
    runStdoutLoggingT (filterLogger f m)
  where
    (Env (Config level)) = getEnv r
    f _ = (>= level)
