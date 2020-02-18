{-# LANGUAGE BlockArguments #-}

module CROE.Backend.Logger.Base
  ( Config
  , Env
  , withEnv
  , runLogger
  , runLoggerPure
  ) where

import           Control.Monad             (when)
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as Map
import           Data.Text                 (Text)
import qualified Data.Text.Encoding        as T
import           Polysemy
import           Polysemy.Output
import           System.Log.FastLogger
import           Text.Printf

import           CROE.Backend.Logger.Class

data Env = Env
  { _env_config    :: Config
  , _env_loggerSet :: TimedFastLogger
  }

newtype Config = Config
  { _config_level :: LogLevel
  }

instance FromJSON Config where
  parseJSON = withObject "Logger.Config" $ \o ->
    Config . readLogLevel <$> o .: "level"

readLogLevel :: Text -> LogLevel
readLogLevel str = Map.findWithDefault LevelDebug str levelFromStr

showLogLevel :: LogLevel -> Text
showLogLevel level = Map.findWithDefault "Other" level levelToStr

levelFromStr :: Map Text LogLevel
levelFromStr = Map.fromList
    [ ("Debug", LevelDebug)
    , ("Info", LevelInfo)
    , ("Warn", LevelWarn)
    , ("Error", LevelError)
    ]

levelToStr :: Map LogLevel Text
levelToStr = Map.fromList . fmap (\(a, b) -> (b, a)) . Map.toList $ levelFromStr

withEnv :: MonadIO m
        => Config -> (Env -> m a) -> m a
withEnv config f = do
    timeCache <- liftIO $ newTimeCache simpleTimeFormat'
    (fastLogger, cleanup) <- liftIO $ newTimedFastLogger timeCache (LogStdout defaultBufSize)
    x <- f (Env config fastLogger)
    liftIO cleanup
    pure x

runLogger :: Member (Embed IO) r
          => Env
          -> Sem (Logger : r) a
          -> Sem r a
runLogger (Env (Config logLevel) logger) = interpret $ \case
    PrintLog level str ->
      let levelStr = showLogLevel level
       in when (level >= logLevel) $ do
            embed $ logger $ \formattedTime ->
              toLogStr (printf "[%s] [%s] " (T.decodeUtf8 formattedTime) levelStr :: String)
              <> toLogStr str
              <> "\n"

runLoggerPure :: Sem (Logger : r) a
              -> Sem r ([(LogLevel, Text)], a)
runLoggerPure =
    runOutputList
  . reinterpret \case
      PrintLog level msg -> do
        let msg' = T.decodeUtf8 . fromLogStr . toLogStr $ msg
        output (level, msg')
