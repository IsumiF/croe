module CROE.Backend.Persist.Internal where

import           Database.Persist.Sql       (SqlBackend)
import           Database.Persist.Types
import           Polysemy

import           CROE.Backend.Persist.Types hiding (migrateAll)

data ConnectionPool m a where
  WithConn :: (SqlBackend -> m a) -> ConnectionPool m a

makeSem ''ConnectionPool

data ReadEntity record (m :: * -> *) a where
  Get :: SqlBackend -> Key record -> ReadEntity record m (Maybe record)
  GetBy :: SqlBackend -> Unique record -> ReadEntity record m (Maybe (Entity record))
  SelectFirst :: SqlBackend -> [Filter record] -> [SelectOpt record] -> ReadEntity record m (Maybe (Entity record))
  SelectList :: SqlBackend -> [Filter record] -> [SelectOpt record] -> ReadEntity record m [Entity record]

makeSem ''ReadEntity

data WriteEntity record (m :: * -> *) a where
  Insert :: SqlBackend -> record -> WriteEntity record m (Key record)
  Insert_ :: SqlBackend -> record -> WriteEntity record m ()
  InsertUnique :: SqlBackend -> record -> WriteEntity record m (Maybe (Key record))
  Replace :: SqlBackend -> Key record -> record -> WriteEntity record m ()
  Update :: SqlBackend -> Key record -> [Update record] -> WriteEntity record m ()
  UpdateWhere :: SqlBackend -> [Filter record] -> [Update record] -> WriteEntity record m ()

makeSem ''WriteEntity
