module CROE.Backend.Service.Task
  ( newTask
  ) where

import           Control.Lens
import           Data.Coerce
import           Data.Int
import qualified Data.Text.Encoding               as T
import           Database.Persist                 ((=.))
import           Database.Persist.Sql             (fromSqlKey, toSqlKey)
import           Polysemy
import           Servant.Server

import           CROE.Backend.Logger.Class
import           CROE.Backend.ObjectStorage.Class
import qualified CROE.Backend.Persist.Class       as Persist
import           CROE.Backend.Persist.Types
import qualified CROE.Common.API.Task             as Common
import qualified CROE.Common.User                 as Common
import           CROE.Common.Util                 (showt)

newTask :: Members '[ ObjectStorage
                    , Persist.ConnectionPool
                    , Persist.Transactional
                    , Persist.ReadEntity User
                    , Persist.WriteEntity Task
                    , Logger
                    ] r
        => Common.User
        -> Common.NewTaskRequest
        -> Sem r (Either ServerError Int64)
newTask authUser request = do
    let currentStatus = coerce Common.TaskStatusReviewing
        reward = request ^. Common.newTaskRequest_reward
        title = request ^. Common.newTaskRequest_title
        location = toSqlKey (request ^. Common.newTaskRequest_campusId)
    user <- Persist.withConn $ \conn ->
      Persist.getBy conn (UniqueUserEmail $ authUser ^. Common.user_email)
    case user of
      Nothing -> pure (Left err404)
      Just userEntity -> do
        let creator = Persist.entityKey userEntity
            duration = request ^. Common.newTaskRequest_duration
            abstract = request ^. Common.newTaskRequest_abstract
            task = Task currentStatus creator Nothing reward title location duration
                     abstract ""
        Persist.withConn $ \conn -> do
          taskId <- Persist.insert conn task
          let taskId' = fromSqlKey taskId
              descriptionKey = "task/" <> showt taskId' <> "/description"
          ok <- putBytes descriptionKey (T.encodeUtf8 (request ^. Common.newTaskRequest_description))
          if not ok
          then do
            Persist.transactionUndo conn
            pure (Left err500)
          else do
            Persist.update conn taskId [TaskDescriptionKey =. descriptionKey]
            pure (Right taskId')
