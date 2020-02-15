module CROE.Backend.Service.Elasticsearch.Class
  ( Elasticsearch(..)
  , putTask
  ) where

import           Data.Text        (Text)
import           Polysemy

import           CROE.Common.Task

data Elasticsearch (m :: * -> *) r where
  PutTask :: Text -- ^id
          -> Task -- ^doc
          -> Elasticsearch m Bool

makeSem ''Elasticsearch
