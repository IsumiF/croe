{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module CROE.Common.Task
  ( Task(..)
  , task_title
  , task_abstract
  , task_reward
  , task_creatorId
  , task_creatorScore
  , task_duration
  , task_location
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
  ) where

import           Control.Lens
import           Data.Aeson
import           Data.Int
import           Data.Map.Strict  (Map)
import qualified Data.Map.Strict  as Map
import           Data.Text        (Text)
import qualified Data.Text        as T
import           Data.Time
import           Data.Word        (Word64)
import           GHC.Generics     (Generic)
import           Text.Read        (readMaybe)

import           CROE.Common.Util (aesonOptions, reverseMap, showt)

data Task = Task
  { _task_title        :: Text
  , _task_abstract     :: Text
  , _task_reward       :: Word64
  , _task_creatorId    :: Int64
  , _task_creatorScore :: Maybe Double
  , _task_duration     :: (UTCTime, UTCTime)
  , _task_location     :: Text
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
                  deriving (Eq, Ord)

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

makeLenses ''Task
makeLenses ''TaskDetail
makePrisms ''TaskStatus
