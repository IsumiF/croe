{-# LANGUAGE TemplateHaskell #-}

module CROE.Frontend.Client.Protected
  ( ProtectedClient(..)
  , protectedClient_task
  , TaskClient(..)
  , taskClient_new
  ) where

import           Control.Lens
import           Data.Int
import           Data.Text
import           Reflex.Dom
import           Servant.Reflex

import           CROE.Common.API.Task

newtype ProtectedClient t m = ProtectedClient
  { _protectedClient_task :: TaskClient t m
  }

newtype TaskClient t m = TaskClient
  { _taskClient_new :: Dynamic t (Either Text NewTaskRequest)
                    -> Event t ()
                    -> m (Event t (ReqResult () Int64))
  }

makeLenses ''ProtectedClient
makeLenses ''TaskClient
