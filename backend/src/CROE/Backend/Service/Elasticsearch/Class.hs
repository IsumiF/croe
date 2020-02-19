module CROE.Backend.Service.Elasticsearch.Class
  ( Elasticsearch(..)
  , searchTask
  , putTask
  , updateTaskStatus
  ) where

import           Data.Text        (Text)
import           Polysemy

import           CROE.Common.Task

data Elasticsearch (m :: * -> *) r where
  SearchTask :: TaskQueryCondition -> Elasticsearch m TaskSearchResult
  PutTask :: Text -- ^id
          -> Task -- ^doc
          -> Elasticsearch m Bool
  UpdateTaskStatus :: Text
                   -> TaskStatus
                   -> Elasticsearch m Bool

makeSem ''Elasticsearch
