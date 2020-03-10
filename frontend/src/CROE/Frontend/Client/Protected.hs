{-# LANGUAGE TemplateHaskell #-}

module CROE.Frontend.Client.Protected
  ( ProtectedClient(..)
  , protectedClient_task
  , protectedClient_school
  , TaskClient(..)
  , taskClient_new
  , taskClient_update
  , taskClient_changeStatus
  , taskClient_get
  , taskClient_search
  , taskClient_reindex
  , SchoolClient(..)
  , schoolClient_get
  ) where

import           Control.Lens
import           Data.Int
import           Data.Text
import           Reflex.Dom
import           Servant.API
import           Servant.Reflex

import           CROE.Common.API.School
import           CROE.Common.API.Task

data ProtectedClient t m = ProtectedClient
  { _protectedClient_task   :: TaskClient t m
  , _protectedClient_school :: SchoolClient t m
  }

data TaskClient t m = TaskClient
  { _taskClient_new :: Dynamic t (Either Text NewTaskRequest)
                    -> Event t ()
                    -> m (Event t (ReqResult () Int64))
  , _taskClient_update :: Dynamic t (Either Text Int64)
                       -> Dynamic t (Either Text NewTaskRequest)
                       -> Event t ()
                       -> m (Event t (ReqResult () NoContent))
  , _taskClient_changeStatus :: Dynamic t (Either Text Int64)
                             -> Dynamic t (QParam TaskAction)
                             -> Event t ()
                             -> m (Event t (ReqResult () NoContent))
  , _taskClient_get :: Dynamic t (Either Text Int64)
                    -> Event t ()
                    -> m (Event t (ReqResult () TaskDetail))
  , _taskClient_search :: Dynamic t (Either Text TaskQueryCondition)
                       -> Event t ()
                       -> m (Event t (ReqResult () TaskSearchResult))
  , _taskClient_reindex :: Event t ()
                        -> m (Event t (ReqResult () NoContent))
  }

newtype SchoolClient t m = SchoolClient
  { _schoolClient_get :: Event t ()
                      -> m (Event t (ReqResult () [School]))
  }

makeLenses ''ProtectedClient
makeLenses ''TaskClient
makeLenses ''SchoolClient
