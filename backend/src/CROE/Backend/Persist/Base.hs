{-# LANGUAGE BangPatterns #-}

module CROE.Backend.Persist.Base
  ( -- *Init
    Config
  , Env
  , withEnv
  -- *Interpreter
  , runConnectionPool
  , runTransactional
  , runReadEntity
  , runWriteEntity
  ) where

import           Control.Monad.IO.Class
import           Control.Monad.Logger          hiding (LogLevel (..))
import qualified Control.Monad.Logger          as MonadLogger
import           Control.Monad.Reader          (runReaderT)
import           Data.Aeson
import           Data.Function                 ((&))
import           Data.Pool
import           Data.Word                     (Word16)
import qualified Database.Persist              as Persist
import           Database.Persist.MySQL        (connectDatabase, connectHost,
                                                connectPassword, connectPort,
                                                connectUser, defaultConnectInfo,
                                                transactionSave, withMySQLPool)
import qualified Database.Persist.MySQL        as MySQL
import           Database.Persist.Sql          (SqlBackend, runMigration)
import           GHC.Generics                  (Generic)
import           Polysemy

import qualified CROE.Backend.Logger.Base      as Logger
import           CROE.Backend.Logger.Class
import           CROE.Backend.Persist.Internal
import           CROE.Backend.Persist.Types    (migrateAll)
import           CROE.Common.Util              (aesonOptions)

data Config = Config
  { _config_host     :: String
  , _config_port     :: Word16
  , _config_user     :: String
  , _config_password :: String
  , _config_database :: String
  , _config_numConns :: Int
  , _config_migrate  :: Bool
  } deriving (Show, Eq, Generic)

instance FromJSON Config where
  parseJSON = genericParseJSON aesonOptions

instance ToJSON Config where
  toJSON = genericToJSON aesonOptions
  toEncoding = genericToEncoding aesonOptions

newtype Env = Env MySQL.ConnectionPool

withEnv :: Config
        -> Logger.Env
        -> (Env -> IO a)
        -> IO a
withEnv config loggerEnv f = runLoggingT result logger'
  where
    result = withEnv' config $ \env -> liftIO $ f env

    logger' :: Loc -> LogSource -> MonadLogger.LogLevel -> LogStr -> IO ()
    logger' loc source level str = logger loc source level str
                                 & Logger.runLogger loggerEnv
                                 & runM

    logger :: Members [Embed IO, Logger] r
           => Loc -> LogSource -> MonadLogger.LogLevel -> LogStr -> Sem r ()
    logger _ _ logLevel = printLog (convertLogLevel logLevel)

    convertLogLevel :: MonadLogger.LogLevel -> LogLevel
    convertLogLevel MonadLogger.LevelDebug = LevelDebug
    convertLogLevel MonadLogger.LevelInfo  = LevelInfo
    convertLogLevel MonadLogger.LevelWarn  = LevelWarn
    convertLogLevel MonadLogger.LevelError = LevelError
    convertLogLevel _                      = LevelDebug

withEnv' :: Config -> (Env -> LoggingT IO a) -> LoggingT IO a
withEnv' config f =
    withMySQLPool connInfo numConns $ \pool -> do
      if _config_migrate config
      then withResource pool $ \conn -> runReaderT (runMigration migrateAll) conn
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

runConnectionPool :: (Member (Embed IO) r)
                  => Env
                  -> Sem (ConnectionPool : r) a
                  -> Sem r a
runConnectionPool (Env pool) = interpretH $ \case
    WithConn connConsumer -> do
      (conn, localPool) <- embed (takeResource pool)
      !x <- runT $ connConsumer conn
      y <- raise (runConnectionPool (Env pool) x)
      embed $ do
        runReaderT transactionSave conn
        putResource localPool conn
      pure y

runTransactional :: (Member (Embed IO) r)
                 => Sem (Transactional : r) a
                 -> Sem r a
runTransactional = interpret $ \case
    TransactionUndo conn -> embed $ runReaderT MySQL.transactionUndo conn

runReadEntity :: ( Member (Embed IO) r
                  , Persist.PersistEntity record
                  , Persist.PersistEntityBackend record ~ SqlBackend
                  )
               => Sem (ReadEntity record : r) a
               -> Sem r a
runReadEntity = interpret $ \case
    Get conn key -> runReaderT (Persist.get key) conn
    GetBy conn key -> runReaderT (Persist.getBy key) conn
    SelectFirst conn a b -> runReaderT (Persist.selectFirst a b) conn
    SelectList conn a b -> runReaderT (Persist.selectList a b) conn

runWriteEntity :: ( Member (Embed IO) r
                   , Persist.PersistEntity record
                   , Persist.PersistEntityBackend record ~ SqlBackend
                   )
                => Sem (WriteEntity record : r) a
                -> Sem r a
runWriteEntity = interpret $ \case
    Insert conn r -> runReaderT (Persist.insert r) conn
    Insert_ conn r -> runReaderT (Persist.insert_ r) conn
    InsertUnique conn r -> runReaderT (Persist.insertUnique r) conn
    Replace conn key r -> runReaderT (Persist.replace key r) conn
    Update conn key updates -> runReaderT (Persist.update key updates) conn
    UpdateWhere conn filters updates -> runReaderT (Persist.updateWhere filters updates) conn
    Upsert conn record updates -> runReaderT (Persist.upsert record updates) conn
