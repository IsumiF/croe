{-# LANGUAGE TemplateHaskell #-}

module CROE.Common.Task
  ( TaskOverview(..)
  , taskOverview_title
  , taskOverview_abstract
  , taskOverview_reward
  , taskOverview_creatorId
  , taskOverview_creatorScore
  , taskOverview_duration
  , taskOverview_location
  , taskOverview_takerId
  , taskOverview_status
  , TaskStatus(..)
  , _TaskStatusReviewing
  , _TaskStatusPublished
  , _TaskStatusAccepted
  , _TaskStatusBeforeFinish
  , _TaskStatusFinished
  , Task(..)
  ) where

import           Control.Lens
import           Data.Int
import           Data.Map.Strict  (Map)
import qualified Data.Map.Strict  as Map
import           Data.Text        (Text)
import           Data.Time
import           Data.Word        (Word64)

import           CROE.Common.Util (reverseMap)

data TaskOverview = TaskOverview
  { _taskOverview_title        :: Text
  , _taskOverview_abstract     :: Text
  , _taskOverview_reward       :: Word64
  , _taskOverview_creatorId    :: Int64
  , _taskOverview_creatorScore :: Double
  , _taskOverview_duration     :: (UTCTime, UTCTime)
  , _taskOverview_location     :: Text
  , _taskOverview_takerId      :: Int64
  , _taskOverview_status       :: TaskStatus
  }

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

data Task = Task
  {
  }

makeLenses ''TaskOverview
makePrisms ''TaskStatus
makeLenses ''Task
