{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

module CROE.Common.API.Task
  ( API
  , NewTaskRequest(..)
  , module CROE.Common.Task
  ) where

import           Control.Lens
import           Data.Aeson
import           Data.Int
import           Data.Text        (Text)
import           Data.Time
import           Data.Word
import           GHC.Generics
import           Servant.API

import           CROE.Common.Task
import           CROE.Common.Util (aesonOptions)

type API = "task" :> APINew

-- |returns task id
type APINew = ReqBody '[JSON] NewTaskRequest :> Post '[JSON] Int64

data NewTaskRequest = NewTaskRequest
  { _newTaskRequest_reward      :: Word64
  , _newTaskRequest_title       :: Text
  , _newTaskRequest_campusId    :: Int64
  , _newTaskRequest_duration    :: (UTCTime, UTCTime)
  , _newTaskRequest_description :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON NewTaskRequest where
  parseJSON = genericParseJSON aesonOptions

instance ToJSON NewTaskRequest where
  toJSON = genericToJSON aesonOptions
  toEncoding = genericToEncoding aesonOptions

makeLenses ''NewTaskRequest
