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

instance (MonadIO m, PersistEntity e, PersistEntityBackend e ~ SqlBackend)
  => ReadEntity e (ReaderT SqlBackend m) where
  getBy = Persist.getBy

class Monad m => WriteEntity record m where
  insertUnique :: record -> m (Maybe (Key record))

instance (MonadIO m, PersistEntity r, PersistEntityBackend r ~ SqlBackend)
  => WriteEntity r (ReaderT SqlBackend m) where
  insertUnique = Persist.insertUnique
