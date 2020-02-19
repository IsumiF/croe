module CROE.Backend.IntTest.APISpec.Task
  ( spec
  ) where

import           Control.Lens
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Data.Default
import           Data.Functor                      (void)
import           Data.Time
import           Servant.API
import           Test.Hspec

import           CROE.Backend.IntTest.APISpec.Base
import           CROE.Common.API.Task

spec :: SpecWith Server
spec =
    describe "TaskDetail" $
      it "CRUD works" $ \server ->
        shouldNoExcept $ do
          let startTime = UTCTime (fromGregorian 2020 1 19) 0
              endTime = UTCTime (fromGregorian 2020 1 20) 0
              newTaskRequest = NewTaskRequest 500 "任务名" 1 (startTime, endTime) "摘要" "一些描述"
          taskId <- liftEitherMShow $ runClientM' server $
            newTask newTaskRequest
          createdTask <- liftEitherMShow $ runClientM' server $
            getTask taskId
          lift $ createdTask ^. taskDetail_title `shouldBe` "任务名"
          lift $ createdTask ^. taskDetail_description `shouldBe` "一些描述"
          void $ liftEitherMShow $ runClientM' server $
            updateTask taskId newTaskRequest
              { _newTaskRequest_description = "修改后的描述"
              , _newTaskRequest_abstract = "修改后的摘要"
              }
          updatedTask <- liftEitherMShow $ runClientM' server $
            getTask taskId
          lift $ updatedTask ^. taskDetail_description `shouldBe` "修改后的描述"
          void $ liftEitherMShow $ runClientM' server $
            publishTask taskId
          publishedTask <- liftEitherMShow $ runClientM' server $
            getTask taskId
          lift $ publishedTask ^. taskDetail_status `shouldBe` TaskStatusPublished
          taskSearchResult <- liftEitherMShow $ runClientM' server $
            searchTask $ def & taskQueryCondition_query .~ "摘要"
                             & taskQueryCondition_limit .~ 1
                             & (taskQueryCondition_status ?~ TaskStatusPublished)
          let total = taskSearchResult ^. taskSearchResult_total
              tasks = taskSearchResult ^. taskSearchResult_tasks
          lift $ do
            total `shouldSatisfy` (>= 1)
            tasks `shouldSatisfy` (\t -> length t == 1)
  where
    authData = BasicAuthData "fengzlin@mail2.sysu.edu.cn" "123"
    taskClient = _protectedClient_task $ _client_protected servantClient authData
    newTask = _taskClient_newTask taskClient
    updateTask = _taskClient_updateTask taskClient
    publishTask = _taskClient_publishTask taskClient
    getTask = _taskClient_getTask taskClient
    searchTask = _taskClient_search taskClient

liftEitherMShow :: (Show e, Functor m) => m (Either e a) -> ExceptT String m a
liftEitherMShow = withExceptT show . ExceptT

shouldNoExcept :: ExceptT String IO a -> Expectation
shouldNoExcept m = do
    r <- runExceptT m
    case r of
      Left errMsg -> expectationFailure errMsg
      Right _     -> pure ()
