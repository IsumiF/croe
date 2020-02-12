{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CROE.Backend.Persist.Types.TaskStatus
  ( TaskStatus(..)
  ) where

import           Database.Persist.TH

import qualified CROE.Common.Task    as Common (TaskStatus (..))

newtype TaskStatus = TaskStatus Common.TaskStatus
  deriving newtype (Show, Read)

derivePersistField "TaskStatus"
