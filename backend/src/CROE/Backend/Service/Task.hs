{-# LANGUAGE QuasiQuotes #-}

module CROE.Backend.Service.Task
  ( newTask
  , updateTask
  , changeStatus
  , getTask
  , searchTask
  , reindex
  , taskAddReview
  ) where

import           Control.Lens                             hiding (Review)
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Maybe
import           Data.Coerce
import           Data.Foldable                            (forM_)
import           Data.Functor                             (void)
import           Data.Int
import           Data.Maybe
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
import qualified CROE.Backend.Persist.Review              as Persist
import           CROE.Backend.Persist.Types
import qualified CROE.Backend.Service.Elasticsearch.Class as ES
import qualified CROE.Common.API.Task                     as Common
import qualified CROE.Common.User                         as Common
import           CROE.Common.Util                         (liftBool, showt)

newTask :: Members '[ ObjectStorage
                    , Persist.ConnectionPool
                    , Persist.Transactional
                    , Persist.ReadEntity User
                    , Persist.ReadEntity Task
                    , Persist.WriteEntity Task
                    , Persist.ReadEntity SchoolCampus
                    , Persist.ReadEntity School
                    , Persist.ReadEntity Review
                    , Persist.ReviewRepo
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

            liftBool $ syncTaskToES conn taskId

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
                       , Persist.ReadEntity Review
                       , Persist.ReadEntity School
                       , Persist.ReadEntity SchoolCampus
                       , Persist.ReadEntity Task
                       , Persist.WriteEntity Task
                       , Persist.ReviewRepo
                       , ObjectStorage
                       , ES.Elasticsearch
                       ] r
           => Common.User
           -> Int64
           -> Common.NewTaskRequest
           -> Sem r (Either ServerError NoContent)
updateTask authUser taskId request = do
    printLogt LevelInfo [i|user #{authUser ^. Common.user_email} is updating task #{taskId}|]
    Persist.withConn $ \conn -> do
      r <- runExceptT $ do
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
              maybeToExceptT err500 $ liftBool $ syncTaskToES conn (toSqlKey taskId)
              pure NoContent
      case r of
        Left _  -> Persist.transactionUndo conn
        Right _ -> pure ()
      pure r
  where
    reward = request ^. Common.newTaskRequest_reward
    title = request ^. Common.newTaskRequest_title
    campusId = toSqlKey $ request ^. Common.newTaskRequest_campusId
    duration = request ^. Common.newTaskRequest_duration
    abstract = request ^. Common.newTaskRequest_abstract
    description = request ^. Common.newTaskRequest_description

changeStatus :: Members '[ Logger
                        , Persist.ConnectionPool
                        , Persist.ReadEntity User
                        , Persist.ReadEntity Task
                        , Persist.WriteEntity Task
                        , Persist.ReadEntity School
                        , Persist.ReadEntity SchoolCampus
                        , Persist.ReadEntity Review
                        , Persist.ReviewRepo
                        , ES.Elasticsearch
                        ] r
            => Common.User
            -> Int64
            -> Maybe Common.TaskAction
            -> Sem r (Either ServerError NoContent)
changeStatus authUser taskId taskAction =
    Persist.withConn $ \conn -> runExceptT $ do
      task <- maybeToExceptT err404 . MaybeT $ Persist.get conn taskId'
      let taskActionMaybe = Common.nextTaskAction (coerce (taskCurrentStatus task)) (toSqlKey (authUser ^. Common.user_id))
            (taskCreator task) (taskTaker task)
      case taskActionMaybe of
        Nothing -> throwE err412
        Just action ->
          if Just action /= taskAction
          then throwE err412 { errBody = "unexpected action" }
          else do
            let updateTaker = if action == Common.TaskActionAccept then [TaskTaker =. Just myId] else []
                oldStatus = taskCurrentStatus task
                newStatus = succ oldStatus
            lift $ Persist.update conn taskId' ([TaskCurrentStatus =. newStatus] <> updateTaker)
            maybeToExceptT err500 $ liftBool $ syncTaskToES conn taskId'
            pure NoContent
  where
    taskId' = toSqlKey taskId
    myId = toSqlKey (authUser ^. Common.user_id)

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

searchTask :: Members '[ ES.Elasticsearch
                       ] r
           => Common.User
           -> Common.TaskQueryCondition
           -> Sem r (Either ServerError Common.TaskSearchResult)
searchTask me queryCondition = do
    let queryCondition' =
          if isCreatorOrTaker
          then queryCondition
          else queryCondition & Common.taskQueryCondition_status ?~ Common.TaskStatusPublished
    searchResult <- ES.searchTask queryCondition'
    pure (Right searchResult)
  where
    myId = me ^. Common.user_id
    creatorId = queryCondition ^. Common.taskQueryCondition_creatorId
    takerId = queryCondition ^. Common.taskQueryCondition_takerId
    isCreatorOrTaker = creatorId == Just myId || takerId == Just myId

reindex :: Members '[ Persist.ConnectionPool
                    , Persist.ReadEntity Task
                    , Persist.ReadEntity School
                    , Persist.ReadEntity SchoolCampus
                    , Persist.ReadEntity Review
                    , Persist.ReadEntity User
                    , Persist.ReviewRepo
                    , ES.Elasticsearch
                    ] r
        => Common.User
        -> Sem r (Either ServerError NoContent)
reindex authUser =
    if role /= Common.RoleAdmin
    then pure (Left err403)
    else
      Persist.withConn $ \conn -> do
        taskIds :: [TaskId] <- Persist.selectKeysList conn [] []
        forM_ taskIds $ \taskId ->
          syncTaskToES conn taskId
        pure (Right NoContent)
  where
    role = authUser ^. Common.user_role

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

syncTaskToES :: Members '[ Persist.ReadEntity Task
                         , Persist.ReadEntity School
                         , Persist.ReadEntity SchoolCampus
                         , Persist.ReadEntity Review
                         , Persist.ReadEntity User
                         , Persist.ReviewRepo
                         , ES.Elasticsearch
                         ] r
             => Persist.Connection
             -> TaskId
             -> Sem r Bool
syncTaskToES conn taskId = fmap isJust $ runMaybeT $ do
    Task{..} <- MaybeT $ Persist.get conn taskId
    campus <- MaybeT $ Persist.get conn taskLocation
    let schoolId = schoolCampusSchoolId campus
    school <- MaybeT $ Persist.get conn schoolId
    let locationStr = schoolName school <> " " <> schoolCampusName campus
    creatorScore <- lift $ Persist.averageRating conn taskCreator
    creator <- MaybeT $ Persist.get conn taskCreator
    reviewsOfTask <- lift $ Persist.selectList conn [ReviewTaskId ==. taskId] []
    let reviewedByUsers = fmap (fromSqlKey . reviewFrom . Persist.entityVal) reviewsOfTask
        esTask = Common.Task
          { Common._task_title = taskTitle
          , Common._task_abstract = taskAbstract
          , Common._task_reward = taskReward
          , Common._task_creatorId = fromSqlKey taskCreator
          , Common._task_creatorName = userName creator
          , Common._task_creatorScore = creatorScore
          , Common._task_duration = taskDuration
          , Common._task_location = locationStr
          , Common._task_campusId = fromSqlKey taskLocation
          , Common._task_takerId = fromSqlKey <$> taskTaker
          , Common._task_status = coerce taskCurrentStatus
          , Common._task_reviewedByUsers = reviewedByUsers
          }
    liftBool $ ES.putTask (showt (fromSqlKey taskId)) esTask

taskAddReview :: Members '[ Persist.ConnectionPool
                          , Persist.WriteEntity Review
                          , Persist.ReadEntity Review
                          , Persist.ReadEntity Task
                          , Persist.ReadEntity SchoolCampus
                          , Persist.ReadEntity School
                          , Persist.ReadEntity User
                          , Persist.ReviewRepo
                          , ES.Elasticsearch
                          ] r
              => Common.User
              -> Int64
              -> Common.TaskAddReview
              -> Sem r (Either ServerError NoContent)
taskAddReview me taskId' Common.TaskAddReview{..} = runExceptT $ do
    task :: Task <- maybeToExceptT err404 . MaybeT . Persist.withConn $ \conn ->
      Persist.get conn taskId
    userIdToReview <- maybeToExceptT err403 . MaybeT . pure $ taskGetTheOtherUser task myId
    let newReview = Review
          { reviewUser = userIdToReview
          , reviewFrom = myId
          , reviewRating = _taskAddReview_score
          , reviewMessage = _taskAddReview_reason
          , reviewTaskId = taskId
          }
    void . maybeToExceptT err412 . MaybeT . Persist.withConn $ \conn ->
      Persist.insertUnique conn newReview
    score <- lift . Persist.withConn $ \conn -> do
      s <- Persist.averageRating conn userIdToReview
      void $ syncTaskToES conn taskId
      pure s
    void . lift $ ES.updateCreatorScore (fromSqlKey userIdToReview) score

    pure NoContent
  where
    myId = toSqlKey (me ^. Common.user_id)
    taskId = toSqlKey taskId'

-- 获取参与该任务的另一方用户
taskGetTheOtherUser :: Task -> UserId -> Maybe UserId
taskGetTheOtherUser task me
  | taskCreator task == me = taskTaker task
  | taskTaker task == Just me = Just (taskCreator task)
  | otherwise = Nothing
