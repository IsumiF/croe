{-# LANGUAGE BangPatterns #-}

module CROE.Backend.Service.Task
  ( newTask
  ) where

import           Control.Lens                             hiding (Review)
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Data.Coerce
import           Data.Int
import qualified Data.Text.Encoding                       as T
import           Database.Persist                         ((=.), (==.))
import           Database.Persist.Sql                     (fromSqlKey, toSqlKey)
import           Polysemy
import           Servant.Server

import           CROE.Backend.Logger.Class
import           CROE.Backend.ObjectStorage.Class
import qualified CROE.Backend.Persist.Class               as Persist
import           CROE.Backend.Persist.Types
import qualified CROE.Backend.Service.Elasticsearch.Class as ES
import qualified CROE.Common.API.Task                     as Common
import qualified CROE.Common.User                         as Common
import           CROE.Common.Util                         (showt, liftBool)

newTask :: Members '[ ObjectStorage
                    , Persist.ConnectionPool
                    , Persist.Transactional
                    , Persist.ReadEntity User
                    , Persist.WriteEntity Task
                    , Persist.ReadEntity SchoolCampus
                    , Persist.ReadEntity School
                    , Persist.ReadEntity Review
                    , ES.Elasticsearch
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
          r <- runMaybeT $ do
            taskId <- lift $ Persist.insert conn task
            let taskId' = fromSqlKey taskId
                descriptionKey = "task/" <> showt taskId' <> "/description"
            liftBool $ putBytes descriptionKey (T.encodeUtf8 (request ^. Common.newTaskRequest_description))

            campus <- MaybeT $ Persist.get conn location
            let schoolId = schoolCampusSchoolId campus
            school <- MaybeT $ Persist.get conn schoolId
            let locationStr = schoolName school <> " " <> schoolCampusName campus
            creatorScore <- lift $ averageRate conn creator
            let esTask = Common.Task title abstract reward (fromSqlKey creator)
                  creatorScore duration locationStr Nothing (coerce currentStatus)
            liftBool $ ES.putTask (showt taskId') esTask

            lift $ Persist.update conn taskId [TaskDescriptionKey =. descriptionKey]
            pure (Right taskId')
          case r of
            Nothing -> do
              Persist.transactionUndo conn
              pure (Left err500)
            Just x -> pure x

averageRate :: Member (Persist.ReadEntity Review) r
            => Persist.Connection
            -> Persist.Key User
            -> Sem r (Maybe Double)
averageRate conn key = do
    entities <- Persist.selectList conn [ReviewUser ==. key] []
    let !ratings = reviewRating . Persist.entityVal <$> entities
    if null ratings
    then pure Nothing
    else pure . Just $ sum ratings / fromIntegral (length ratings)
