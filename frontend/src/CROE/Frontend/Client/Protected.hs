{-# LANGUAGE TemplateHaskell #-}

module CROE.Frontend.Client.Protected
  ( ProtectedClient(..)
  , protectedClient_task
  , TaskClient(..)
  , taskClient_new
  , taskClient_update
  , taskClient_publish
  , taskClient_get
  ) where

import           Control.Lens
import           Data.Int
import           Data.Text
import           Reflex.Dom
import           Servant.API
import           Servant.Reflex

import           CROE.Common.API.Task

newtype ProtectedClient t m = ProtectedClient
  { _protectedClient_task :: TaskClient t m
  }

data TaskClient t m = TaskClient
  { _taskClient_new :: Dynamic t (Either Text NewTaskRequest)
                    -> Event t ()
                    -> m (Event t (ReqResult () Int64))
  , _taskClient_update :: Dynamic t (Either Text Int64)
                       -> Dynamic t (Either Text NewTaskRequest)
                       -> Event t ()
                       -> m (Event t (ReqResult () NoContent))
  , _taskClient_publish :: Dynamic t (Either Text Int64)
                        -> Event t ()
                        -> m (Event t (ReqResult () NoContent))
  , _taskClient_get :: Dynamic t (Either Text Int64)
                    -> Event t ()
                    -> m (Event t (ReqResult () TaskDetail))
  }

makeLenses ''ProtectedClient
makeLenses ''TaskClient
