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
import           Data.Time
import           Data.Word        (Word64)

import           CROE.Common.Util (reverseMap, showt)

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
  } deriving (Show, Eq)

data TaskStatus = TaskStatusReviewing
                | TaskStatusPublished
                | TaskStatusAccepted
                | TaskStatusBeforeFinish
                | TaskStatusFinished
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

makeLenses ''Task
makePrisms ''TaskStatus
