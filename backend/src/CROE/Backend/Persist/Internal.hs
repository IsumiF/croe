module CROE.Backend.Persist.Internal where

import           Database.Persist.Sql       (SqlBackend)
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
