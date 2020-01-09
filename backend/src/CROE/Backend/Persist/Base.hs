{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}

module CROE.Backend.Persist.Base
  (
  -- *Interface
    MonadPersist(..)
  -- *Init
  , Config
  , Env
  , withEnv
  , HasEnv(..)
  ) where

import           Control.Monad.IO.Unlift
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Data.Aeson
import           Data.Pool                   (withResource)
import           Data.Word                   (Word16)
import           Database.Persist.MySQL
import           GHC.Generics                (Generic)

import           CROE.Backend.Persist.Types  (migrateAll)
import           CROE.Common.Util            (aesonOptions)

class Monad m => MonadPersist backend m | m -> backend where
  withConn :: ReaderT backend m a -> m a

instance (HasEnv r, MonadBaseControl IO m) => MonadPersist SqlBackend (ReaderT r m) where
  withConn m = do
    (Env pool) <- asks getEnv
    withResource pool $ \conn ->
      runReaderT m conn

data Config = Config
  { _config_host     :: String
  , _config_port     :: Word16
  , _config_user     :: String
  , _config_password :: String
  , _config_database :: String
  , _config_numConns :: Int
  , _config_migrate  :: Bool
  } deriving (Show, Eq, Generic)

newtype Env = Env ConnectionPool

class HasEnv r where
  getEnv :: r -> Env

instance HasEnv Env where
  getEnv = id

instance FromJSON Config where
  parseJSON = genericParseJSON aesonOptions

instance ToJSON Config where
  toJSON = genericToJSON aesonOptions
  toEncoding = genericToEncoding aesonOptions

withEnv :: (MonadLogger m, MonadUnliftIO m, MonadBaseControl IO m)
        => Config -> (Env -> m a) -> m a
withEnv config f =
    withMySQLPool connInfo numConns $ \pool -> do
      if _config_migrate config
      then runReaderT (withConn $ runMigration migrateAll) (Env pool)
      else pure ()
      f (Env pool)
  where
    connInfo = defaultConnectInfo
      { connectHost = _config_host config
      , connectPort = _config_port config
      , connectUser = _config_user config
      , connectPassword = _config_password config
      , connectDatabase = _config_database config
      }
    numConns = _config_numConns config
