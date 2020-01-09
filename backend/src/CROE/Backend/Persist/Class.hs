{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module CROE.Backend.Persist.Class
  ( MonadPersist(..)
  , module CROE.Backend.Persist.Types
  , ReadEntity(..)
  ) where

import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Database.Persist           hiding (getBy)
import qualified Database.Persist           as Persist
import           Database.Persist.Sql

import           CROE.Backend.Persist.Base  (MonadPersist (..))
import           CROE.Backend.Persist.Types

class Monad m => ReadEntity e m where
  getBy :: Unique e -> m (Maybe (Entity e))

instance (MonadIO m, PersistEntity e, PersistEntityBackend e ~ SqlBackend)
  => ReadEntity e (ReaderT SqlBackend m) where
  getBy = Persist.getBy
