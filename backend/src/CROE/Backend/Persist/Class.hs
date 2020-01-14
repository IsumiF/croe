{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module CROE.Backend.Persist.Class
  ( MonadPersist(..)
  , module CROE.Backend.Persist.Types
  , ReadEntity(..)
  , WriteEntity(..)
  ) where

import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Database.Persist
import qualified Database.Persist           as Persist
import           Database.Persist.Sql

import           CROE.Backend.Persist.Base  (MonadPersist (..))
import           CROE.Backend.Persist.Types

class Monad m => ReadEntity record m where
  getBy :: Unique record -> m (Maybe (Entity record))
  selectFirst :: [Filter record] -> [SelectOpt record] -> m (Maybe (Entity record))
  selectList :: [Filter record] -> [SelectOpt record] -> m [Entity record]

instance (MonadIO m, PersistEntity e, PersistEntityBackend e ~ SqlBackend)
  => ReadEntity e (ReaderT SqlBackend m) where
  getBy = Persist.getBy
  selectFirst = Persist.selectFirst
  selectList = Persist.selectList

class Monad m => WriteEntity record m where
  insert :: record -> m (Key record)
  insert_ :: record -> m ()
  insertUnique :: record -> m (Maybe (Key record))
  replace :: Key record -> record -> m ()
  update :: Key record -> [Update record] -> m ()
  updateWhere :: [Filter record] -> [Update record] -> m ()

instance (MonadIO m, PersistEntity r, PersistEntityBackend r ~ SqlBackend)
  => WriteEntity r (ReaderT SqlBackend m) where
  insert = Persist.insert
  insert_ = Persist.insert_
  insertUnique = Persist.insertUnique
  replace = Persist.replace
  update = Persist.update
  updateWhere = Persist.updateWhere
