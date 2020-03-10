{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE TemplateHaskell   #-}

module CROE.Common.Task
  ( TaskQueryCondition(..)
  , taskQueryCondition_query
  , taskQueryCondition_rewardRange
  , taskQueryCondition_campusId
  , taskQueryCondition_status
  , taskQueryCondition_creatorId
  , taskQueryCondition_takerId
  , taskQueryCondition_limit
  , taskQueryCondition_offset
  , TaskSearchResult(..)
  , taskSearchResult_total
  , taskSearchResult_tasks
  , emptyTaskSearchResult
  , Task(..)
  , task_title
  , task_abstract
  , task_reward
  , task_creatorId
  , task_creatorName
  , task_creatorScore
  , task_duration
  , task_location
  , task_campusId
  , task_takerId
  , task_status
  , TaskDetail(..)
  , taskDetail_title
  , taskDetail_abstract
  , taskDetail_description
  , taskDetail_status
  , taskDetail_campusId
  , taskDetail_duration
  , taskDetail_reward
  , taskDetail_creatorId
  , taskDetail_takerId
  , TaskStatus(..)
  , _TaskStatusReviewing
  , _TaskStatusPublished
  , _TaskStatusAccepted
  , _TaskStatusBeforeFinish
  , _TaskStatusFinished
  , TaskAction(..)
  , _TaskActionPublish
  , _TaskActionAccept
  , _TaskActionSubmit
  , _TaskActionConfirm
  , nextAction
  , nextTaskAction
  , taskEditable
  ) where

import           Control.Lens
import           Data.Aeson
import           Data.Bifunctor
import           Data.Default
import           Data.Int
import           Data.Map.Strict  (Map)
import qualified Data.Map.Strict  as Map
import           Data.Text        (Text)
import qualified Data.Text        as T
import           Data.Time
import           Data.Word        (Word64)
import           GHC.Generics     (Generic)
import           Servant.API
import           Text.Read        (readEither, readMaybe)

import           CROE.Common.Util (aesonOptions, reverseMap, showt)

data TaskQueryCondition = TaskQueryCondition
  { _taskQueryCondition_query       :: Text
  , _taskQueryCondition_rewardRange :: Maybe (Word64, Word64)
  , _taskQueryCondition_campusId    :: Maybe Int64
  , _taskQueryCondition_status      :: Maybe TaskStatus
  , _taskQueryCondition_creatorId   :: Maybe Int64
  , _taskQueryCondition_takerId     :: Maybe Int64
  , _taskQueryCondition_limit       :: Integer
  , _taskQueryCondition_offset      :: Integer
  } deriving (Show, Eq, Generic)

instance Default TaskQueryCondition where
  def = TaskQueryCondition
    { _taskQueryCondition_query = ""
    , _taskQueryCondition_rewardRange = Nothing
    , _taskQueryCondition_campusId = Nothing
    , _taskQueryCondition_status = Nothing
    , _taskQueryCondition_creatorId = Nothing
    , _taskQueryCondition_takerId = Nothing
    , _taskQueryCondition_limit = 20
    , _taskQueryCondition_offset = 0
    }

instance FromJSON TaskQueryCondition where
  parseJSON = genericParseJSON aesonOptions

instance ToJSON TaskQueryCondition where
  toJSON = genericToJSON aesonOptions
  toEncoding = genericToEncoding aesonOptions

data TaskSearchResult = TaskSearchResult
  { _taskSearchResult_total :: Integer
  , _taskSearchResult_tasks :: [(Text, Task)]
  } deriving (Show, Eq, Generic)

emptyTaskSearchResult :: TaskSearchResult
emptyTaskSearchResult = TaskSearchResult 0 []

instance FromJSON TaskSearchResult where
  parseJSON = genericParseJSON aesonOptions

instance ToJSON TaskSearchResult where
  toJSON = genericToJSON aesonOptions
  toEncoding = genericToEncoding aesonOptions

data Task = Task
  { _task_title        :: Text
  , _task_abstract     :: Text
  , _task_reward       :: Word64
  , _task_creatorId    :: Int64
  , _task_creatorName  :: Text
  , _task_creatorScore :: Maybe Double
  , _task_duration     :: (UTCTime, UTCTime)
  , _task_location     :: Text -- 学校名+校区名
  , _task_campusId     :: Int64
  , _task_takerId      :: Maybe Int64
  , _task_status       :: TaskStatus
  } deriving (Show, Eq, Generic)

instance FromJSON Task where
  parseJSON = genericParseJSON aesonOptions

instance ToJSON Task where
  toJSON = genericToJSON aesonOptions
  toEncoding = genericToEncoding aesonOptions

data TaskDetail = TaskDetail
  { _taskDetail_title       :: Text
  , _taskDetail_abstract    :: Text
  , _taskDetail_description :: Text
  , _taskDetail_status      :: TaskStatus
  , _taskDetail_campusId    :: Int64
  , _taskDetail_duration    :: (UTCTime, UTCTime)
  , _taskDetail_reward      :: Word64
  , _taskDetail_creatorId   :: Int64
  , _taskDetail_takerId     :: Maybe Int64
  } deriving (Show, Eq, Generic)

instance FromJSON TaskDetail where
  parseJSON = genericParseJSON aesonOptions

instance ToJSON TaskDetail where
  toJSON = genericToJSON aesonOptions
  toEncoding = genericToEncoding aesonOptions

data TaskStatus = TaskStatusReviewing -- ^草稿
                | TaskStatusPublished -- ^已发布
                | TaskStatusAccepted -- ^已接受
                | TaskStatusBeforeFinish -- ^接受者认为已完成
                | TaskStatusFinished -- ^发布者确认已完成
                  deriving (Eq, Ord, Enum)

statusToStr :: Map TaskStatus String
statusToStr = Map.fromList
    [ (TaskStatusReviewing, "reviewing")
    , (TaskStatusPublished, "published")
    , (TaskStatusAccepted, "accepted")
    , (TaskStatusBeforeFinish, "before_finish")
    , (TaskStatusFinished, "finished")
    ]

strToStatus :: Map String TaskStatus
strToStatus = reverseMap statusToStr

instance Show TaskStatus where
  show = (statusToStr Map.!)

instance Read TaskStatus where
  readsPrec _ str = maybe [] (\r -> [(r, "")]) (strToStatus Map.!? str)

instance ToJSON TaskStatus where
  toJSON a = String (showt a)

instance FromJSON TaskStatus where
  parseJSON = withText "TaskStatus" $ \t ->
    case readMaybe (T.unpack t) of
      Just stat -> pure stat
      Nothing   -> fail "unexpected value"

data TaskAction = TaskActionPublish
                | TaskActionAccept
                | TaskActionSubmit
                | TaskActionConfirm
                  deriving (Eq, Ord, Generic, Show, Read)

instance ToJSON TaskAction where
  toJSON = genericToJSON aesonOptions
  toEncoding = genericToEncoding aesonOptions

instance FromJSON TaskAction where
  parseJSON = genericParseJSON aesonOptions

instance FromHttpApiData TaskAction where
  parseQueryParam = first T.pack . readEither . T.unpack

instance ToHttpApiData TaskAction where
  toQueryParam = showt

nextAction :: TaskStatus -> TaskAction
nextAction TaskStatusReviewing    = TaskActionPublish
nextAction TaskStatusPublished    = TaskActionAccept
nextAction TaskStatusAccepted     = TaskActionSubmit
nextAction TaskStatusBeforeFinish = TaskActionConfirm

taskEditable :: Eq userId
             => userId -- ^current user id
             -> userId -- ^creator id
             -> TaskStatus
             -> Bool
taskEditable userId creatorId status =
    userId == creatorId && status == TaskStatusReviewing

nextTaskAction :: Eq userId
               => TaskStatus -- ^current status
               -> userId -- ^current user id
               -> userId -- ^creator id
               -> Maybe userId -- ^taker id
               -> Maybe TaskAction
nextTaskAction status userId creatorId takerId =
  case status of
    TaskStatusReviewing ->
      if userId == creatorId then Just TaskActionPublish else Nothing
    TaskStatusPublished ->
      if userId /= creatorId then Just TaskActionAccept else Nothing
    TaskStatusAccepted ->
      if Just userId == takerId then Just TaskActionSubmit else Nothing
    TaskStatusBeforeFinish ->
      if userId == creatorId then Just TaskActionConfirm else Nothing
    TaskStatusFinished -> Nothing

makeLenses ''TaskQueryCondition
makeLenses ''TaskSearchResult
makeLenses ''Task
makeLenses ''TaskDetail
makePrisms ''TaskStatus
makePrisms ''TaskAction
