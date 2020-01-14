{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}

module CROE.Backend.Env
  ( Env
  , withEnv
  , Config
  , readConfig
  , App
  , runApp
  ) where

import           Control.Monad.Except
import           Control.Monad.Logger
import           Control.Monad.Random.Class
import           Control.Monad.Reader
import qualified Crypto.Random              as Crypto
import           Data.Aeson
import           Data.Bifunctor
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Lazy       as LBS
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           GHC.Generics               (Generic)

import qualified CROE.Backend.Logger        as Logger
import qualified CROE.Backend.Mail.Init     as Mail
import qualified CROE.Backend.Persist.Base  as Persist
import           CROE.Common.Util           (aesonOptions)

data Env = Env
  { _env_persist :: Persist.Env
  , _env_logger  :: Logger.Env
  , _env_mail    :: Mail.Env
  }

data Config = Config
  { _config_persist :: Persist.Config
  , _config_logger  :: Logger.Config
  , _config_mail    :: Mail.Config
  } deriving Generic

readConfig :: ByteString -> Either Text Config
readConfig = first T.pack . eitherDecode' . LBS.fromStrict

instance FromJSON Config where
  parseJSON = genericParseJSON aesonOptions

withEnv :: Config -> (Env -> IO a) -> IO a
withEnv c f =
    Logger.withEnv (_config_logger c) $ \_env_logger ->
    Logger.runLoggingT _env_logger $
    Persist.withEnv (_config_persist c) $ \_env_persist ->
      let _env_mail = Mail.newEnv (_config_mail c)
          env = Env{..}
       in liftIO (f env)

type App = ReaderT Env (LoggingT IO)

runApp :: Env -> App a -> IO a
runApp env app = Logger.runLoggingT env $ runReaderT app env

instance Logger.HasEnv Env where
  getEnv = _env_logger

instance Persist.HasEnv Env where
  getEnv = _env_persist

instance Mail.HasEnv Env where
  getEnv = _env_mail

-- Orphan Instances

instance MonadRandom m => MonadRandom (LoggingT m) where
  getRandomR r = lift $ getRandomR r
  getRandom = lift getRandom
  getRandomRs r = lift $ getRandomRs r
  getRandoms = lift getRandoms

instance MonadIO m => Crypto.MonadRandom (ReaderT r m) where
  getRandomBytes n = liftIO $ Crypto.getRandomBytes n

instance Crypto.MonadRandom m => Crypto.MonadRandom (ExceptT e m) where
  getRandomBytes n = lift $ Crypto.getRandomBytes n
