module CROE.Backend.Service.Task
  ( newTask
  ) where

import           Data.Int
import           Polysemy
import           Servant.Server

-- import           CROE.Backend.Persist.Types
import qualified CROE.Common.API.Task       as Common
import qualified CROE.Common.User           as Common

newTask :: Common.User -> Common.NewTaskRequest -> Sem r (Either ServerError Int64)
newTask = undefined
