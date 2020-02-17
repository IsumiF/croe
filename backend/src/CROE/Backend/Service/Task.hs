{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuasiQuotes  #-}

module CROE.Backend.Service.Task
  ( newTask
  , updateTask
  , publishTask
  , getTask
  ) where

import           Control.Lens                             hiding (Review)
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Maybe
import           Data.Coerce
import           Data.Int
import           Data.String.Interpolate
import qualified Data.Text.Encoding                       as T
import           Database.Persist                         ((=.), (==.))
import           Database.Persist.Sql                     (fromSqlKey, toSqlKey)
import           Polysemy
import           Servant.API                              (NoContent (..))
import           Servant.Server

import           CROE.Backend.Logger.Class
import           CROE.Backend.ObjectStorage.Class
import qualified CROE.Backend.Persist.Class               as Persist
import           CROE.Backend.Persist.Types
import qualified CROE.Backend.Service.Elasticsearch.Class as ES
import qualified CROE.Common.API.Task                     as Common
import qualified CROE.Common.User                         as Common
import           CROE.Common.Util                         (liftBool, showt)

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

updateTask :: Members '[ Logger
                       , Persist.ConnectionPool
                       , Persist.Transactional
                       , Persist.ReadEntity User
                       , Persist.ReadEntity SchoolCampus
                       , Persist.ReadEntity Task
                       , Persist.WriteEntity Task
                       , ObjectStorage
                       , ES.Elasticsearch
                       ] r
           => Common.User
           -> Int64
           -> Common.NewTaskRequest
           -> Sem r (Either ServerError NoContent)
updateTask authUser taskId request =
    Persist.withConn $ \conn -> runExceptT $ do
      _ <- maybeToExceptT err404 . MaybeT $ Persist.get conn campusId
      oldTask <- maybeToExceptT err404 . MaybeT $ Persist.get conn (toSqlKey taskId)
      if taskCurrentStatus oldTask /= coerce Common.TaskStatusReviewing
      then throwE err412
      else do
        owns <- lift $ ownsTask conn authUser oldTask
        if not owns
        then throwE err403
        else do
          let descriptionKey = taskDescriptionKey oldTask
          ok <- lift $ putBytes descriptionKey (T.encodeUtf8 description)
          if not ok
          then throwE err500
          else do
            lift $ Persist.update conn (toSqlKey taskId)
              [ TaskReward =. reward
              , TaskTitle =. title
              , TaskLocation =. campusId
              , TaskDuration =. duration
              , TaskAbstract =. abstract
              , TaskDescriptionKey =. descriptionKey
              ]
            pure NoContent
  where
    reward = request ^. Common.newTaskRequest_reward
    title = request ^. Common.newTaskRequest_title
    campusId = toSqlKey $ request ^. Common.newTaskRequest_campusId
    duration = request ^. Common.newTaskRequest_duration
    abstract = request ^. Common.newTaskRequest_abstract
    description = request ^. Common.newTaskRequest_description

publishTask :: Members '[ Logger
                        , Persist.ConnectionPool
                        , Persist.ReadEntity User
                        , Persist.ReadEntity Task
                        , Persist.WriteEntity Task
                        ] r
            => Common.User
            -> Int64
            -> Sem r (Either ServerError NoContent)
publishTask authUser taskId =
    Persist.withConn $ \conn -> runExceptT $ do
      task <- maybeToExceptT err404 . MaybeT $ Persist.get conn taskId'
      owns <- lift $ ownsTask conn authUser task
      if not owns
      then throwE err403
      else
        if taskCurrentStatus task /= coerce Common.TaskStatusReviewing
        then throwE err412
        else do
          lift $ Persist.update conn taskId' [TaskCurrentStatus =. coerce Common.TaskStatusPublished]
          pure NoContent
  where
    taskId' = toSqlKey taskId

getTask :: Members '[ Logger
                    , Persist.ConnectionPool
                    , Persist.ReadEntity Task
                    , ObjectStorage
                    ] r
        => Common.User
        -> Int64
        -> Sem r (Either ServerError Common.TaskDetail)
getTask _ taskId =
    Persist.withConn $ \conn -> runExceptT $ do
      task <- maybeToExceptT err404 . MaybeT $ Persist.get conn taskId'
      let descriptionKey = taskDescriptionKey task
      description <- catchE (maybeToExceptT err500 . MaybeT $ getBytes descriptionKey) $ \e -> do
        lift $ printLogt LevelError [i|fail to read description of task #{taskId}|]
        throwE e
      let result = Common.TaskDetail
            { Common._taskDetail_title = taskTitle task
            , Common._taskDetail_abstract = taskAbstract task
            , Common._taskDetail_description = T.decodeUtf8 description
            , Common._taskDetail_status = coerce (taskCurrentStatus task)
            , Common._taskDetail_campusId = fromSqlKey (taskLocation task)
            , Common._taskDetail_duration = taskDuration task
            , Common._taskDetail_reward = taskReward task
            , Common._taskDetail_creatorId = fromSqlKey (taskCreator task)
            , Common._taskDetail_takerId = fromSqlKey <$> taskTaker task
            }
      pure result
  where
    taskId' = toSqlKey taskId

ownsTask :: Member (Persist.ReadEntity User) r
         => Persist.Connection
         -> Common.User
         -> Task
         -> Sem r Bool
ownsTask conn user task = do
    creatorMaybe <- Persist.get conn (taskCreator task)
    case creatorMaybe of
      Nothing      -> pure False
      Just creator -> pure $ userEmail creator == authEmail
  where
    authEmail = user ^. Common.user_email

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
