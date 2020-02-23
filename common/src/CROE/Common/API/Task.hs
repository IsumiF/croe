{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

module CROE.Common.API.Task
  ( API
  , NewTaskRequest(..)
  , newTaskRequest_reward
  , newTaskRequest_title
  , newTaskRequest_campusId
  , newTaskRequest_duration
  , newTaskRequest_abstract
  , newTaskRequest_description
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

type API = "task" :>
  ( APINew
  :<|> APIUpdate
  :<|> APIPublish
  :<|> APIGet
  :<|> APISearch
  )

-- |returns task id
type APINew = ReqBody '[JSON] NewTaskRequest :> Post '[JSON] Int64

type APIUpdate = Capture "id" Int64
  :> ReqBody '[JSON] NewTaskRequest
  :> Put '[JSON] NoContent

type APIPublish = Capture "id" Int64
  :> "publish"
  :> Post '[JSON] NoContent

type APIGet = Capture "id" Int64
  :> Get '[JSON] TaskDetail

type APISearch = ReqBody '[JSON] TaskQueryCondition
  :> Get '[JSON] TaskSearchResult

data NewTaskRequest = NewTaskRequest
  { _newTaskRequest_title       :: Text
  , _newTaskRequest_reward      :: Word64
  , _newTaskRequest_campusId    :: Int64
  , _newTaskRequest_duration    :: (UTCTime, UTCTime)
  , _newTaskRequest_abstract    :: Text
  , _newTaskRequest_description :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON NewTaskRequest where
  parseJSON = genericParseJSON aesonOptions

instance ToJSON NewTaskRequest where
  toJSON = genericToJSON aesonOptions
  toEncoding = genericToEncoding aesonOptions

makeLenses ''NewTaskRequest
