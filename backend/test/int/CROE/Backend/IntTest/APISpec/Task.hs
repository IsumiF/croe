module CROE.Backend.IntTest.APISpec.Task
  ( spec
  ) where

import           Data.Either
import           Data.Time
import           Servant.API
import           Test.Hspec

import           CROE.Backend.IntTest.APISpec.Base
import           CROE.Common.API.Task

spec :: SpecWith Server
spec =
    describe "newTask" $
      it "returns 200" $ \server -> do
        let startTime = UTCTime (fromGregorian 2020 1 19) 0
            endTime = UTCTime (fromGregorian 2020 1 20) 0
        r <- runClientM' server $ newTask (NewTaskRequest 500 "任务名" 1 (startTime, endTime) "摘要" "一些描述")
        r `shouldSatisfy` isRight
  where
    authData = BasicAuthData "fengzlin@mail2.sysu.edu.cn" "123"
    taskClient = _protectedClient_task $ _client_protected servantClient authData
    newTask = _taskClient_newTask taskClient
