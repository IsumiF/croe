module CROE.Backend.Service.Elasticsearch.Class
  ( Elasticsearch(..)
  , searchTask
  , putTask
  , updateTaskStatus
  , updateCreatorName
  , updateCreatorScore
  ) where

import           Data.Int
import           Data.Text        (Text)
import           Polysemy

import           CROE.Common.Task

data Elasticsearch (m :: * -> *) r where
  SearchTask :: TaskQueryCondition -> Elasticsearch m TaskSearchResult
  PutTask :: Text -- ^id
          -> Task -- ^doc
          -> Elasticsearch m Bool
  UpdateTaskStatus :: Text -- ^task id
                   -> TaskStatus
                   -> Elasticsearch m Bool
  UpdateCreatorName :: Int64 -- ^creator id
                    -> Text -- ^creator name
                    -> Elasticsearch m Bool
  UpdateCreatorScore :: Int64 -- ^creator id
                     -> Maybe Double -- ^creator score
                     -> Elasticsearch m Bool

makeSem ''Elasticsearch
