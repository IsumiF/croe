module CROE.Backend.Persist.Types.TaskStatus
  ( TaskStatus(..)
  ) where

import           Database.Persist.TH

import qualified CROE.Common.Task    as Common (TaskStatus (..))

newtype TaskStatus = TaskStatus Common.TaskStatus
  deriving (Show, Read)

derivePersistField "TaskStatus"
