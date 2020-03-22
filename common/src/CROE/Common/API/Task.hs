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
  , TaskAddReview(..)
  , taskAddReview_score
  , taskAddReview_reason
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
  :<|> APIChangeStatus
  :<|> APIGet
  :<|> APISearch
  :<|> APIReindex
  :<|> APIAddReview
  )

-- |returns task id
type APINew = ReqBody '[JSON] NewTaskRequest :> Post '[JSON] Int64

type APIUpdate = Capture "id" Int64
  :> ReqBody '[JSON] NewTaskRequest
  :> Put '[JSON] NoContent

type APIChangeStatus = Capture "id" Int64
  :> "change_status"
  :> QueryParam "action" TaskAction
  :> Post '[JSON] NoContent

type APIGet = Capture "id" Int64
  :> Get '[JSON] TaskDetail

type APISearch = "search" :> ReqBody '[JSON] TaskQueryCondition
  :> Post '[JSON] TaskSearchResult

type APIReindex = "reindex" :> Post '[JSON] NoContent

type APIAddReview = Capture "id" Int64 :> "review"
  :> ReqBody '[JSON] TaskAddReview
  :> Put '[JSON] NoContent

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

data TaskAddReview = TaskAddReview
  { _taskAddReview_score  :: Double
  , _taskAddReview_reason :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON TaskAddReview where
  parseJSON = genericParseJSON aesonOptions

instance ToJSON TaskAddReview where
  toJSON = genericToJSON aesonOptions
  toEncoding = genericToEncoding aesonOptions

makeLenses ''NewTaskRequest
makeLenses ''TaskAddReview
