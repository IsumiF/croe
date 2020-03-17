module CROE.Backend.Persist.Internal where

import           Control.Monad.Reader
import           Data.Text                  (Text)
import           Database.Persist.Sql       (RawSql, SqlBackend)
import qualified Database.Persist.Sql       as Persist
import           Database.Persist.Types
import           Polysemy

import           CROE.Backend.Persist.Types hiding (migrateAll)

data ConnectionPool m a where
  WithConn :: (Connection -> m a) -> ConnectionPool m a

type Connection = SqlBackend

makeSem ''ConnectionPool

data Transactional (m :: * -> *) a where
  TransactionUndo :: Connection -> Transactional m ()

makeSem ''Transactional

data ReadEntity record (m :: * -> *) a where
  Get :: Connection -> Key record -> ReadEntity record m (Maybe record)
  GetBy :: Connection -> Unique record -> ReadEntity record m (Maybe (Entity record))
  SelectFirst :: Connection -> [Filter record] -> [SelectOpt record] -> ReadEntity record m (Maybe (Entity record))
  SelectList :: Connection -> [Filter record] -> [SelectOpt record] -> ReadEntity record m [Entity record]
  SelectKeysList :: Connection -> [Filter record] -> [SelectOpt record] -> ReadEntity record m [Key record]
  Count :: Connection -> [Filter record] -> ReadEntity record m Int

makeSem ''ReadEntity

data WriteEntity record (m :: * -> *) a where
  Insert :: Connection -> record -> WriteEntity record m (Key record)
  Insert_ :: Connection -> record -> WriteEntity record m ()
  InsertUnique :: Connection -> record -> WriteEntity record m (Maybe (Key record))
  Replace :: Connection -> Key record -> record -> WriteEntity record m ()
  Update :: Connection -> Key record -> [Update record] -> WriteEntity record m ()
  UpdateWhere :: Connection -> [Filter record] -> [Update record] -> WriteEntity record m ()
  Upsert :: Connection -> record -> [Update record] -> WriteEntity record m (Entity record)

makeSem ''WriteEntity

data RawSqlRunner (m :: * -> *) a where
  RawSql :: RawSql b => Connection -> Text -> [PersistValue] -> RawSqlRunner m [b]
  RawExecuteCount :: Connection -> Text -> [PersistValue] -> RawSqlRunner m Int

makeSem ''RawSqlRunner

runRawSqlRunner :: Member (Embed IO) r
                => Sem (RawSqlRunner : r) a
                -> Sem r a
runRawSqlRunner = interpret $ \case
    RawSql conn sql args -> runReaderT (Persist.rawSql sql args) conn
    RawExecuteCount conn sql args -> fromIntegral <$> runReaderT (Persist.rawExecuteCount sql args) conn
