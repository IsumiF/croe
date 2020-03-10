{-# LANGUAGE RecursiveDo #-}

module CROE.Frontend.Widget.TaskList.ViewTask
  ( viewTaskWidget
  , showTaskStatus
  , showCreatorScore
  ) where

import           Control.Lens
import           Control.Monad.IO.Class
import           CROE.Common.Task
import           CROE.Common.User
import           CROE.Common.Util                  (showt)
import           CROE.Frontend.Client
import           Data.Int
import           Data.Text                         (Text)
import qualified Data.Text                         as T
import           Data.Time
import           Reflex.Dom
import           Reflex.Dom.Bulma.Component.Button
import           Text.Printf

viewTaskWidget :: MonadWidget t m
               => User
               -> (Int64, Task)
               -> TaskClient t m
               -> m (Event t (), Event t ()) -- ^(go back, edit)
viewTaskWidget user (taskId, task) client =
    elClass "section" "section" $
      divClass "columns" $ do
        tz <- liftIO getCurrentTimeZone
        divClass "column is-2" blank
        divClass "column is-8" $ mdo
          (backEvt, editEvt) <- divClass "columns" $ do
            divClass "column" $ do
              elClass "h1" "title" $ text (task ^. task_title)
              elClass "h4" "subtitle" $ text (task ^. task_abstract)
              el "div" $ elClass "span" "tag is-medium is-info" $
                text $ "状态：" <> showTaskStatus (task ^. task_status)
              elClass "div" "view-task-tag-2nd" $ elClass "div" "tag is-medium is-success" $
                text $ "奖金：" <> showt (task ^. task_reward)
              elAttr "nav" ("class" =: "level view-task-tag-2nd" <> "id" =: "view-task-level") $ do
                divClass "level-left" $ do
                  divClass "level-item" $
                    elClass "span" "tag is-medium is-link" $
                      text $ "发布人：" <> "fengzelin.isumi"
                  divClass "level-item" $
                    elClass "span" "tag is-medium is-info" $
                      text $ "评分：" <> showCreatorScore (task ^. task_creatorScore)
                divClass "level-right" blank
              elClass "nav" "level" $ do
                divClass "level-left" $ do
                  divClass "level-item" $
                    elClass "span" "tag is-medium is-light" $
                      text $ showDuration tz (task ^. task_duration)
                  divClass "level-item" $
                    elClass "span" "tag is-medium is-light" $
                      text (task ^. task_location)
                divClass "level-right" blank
            divClass "column" $
              divClass "is-pulled-right" $ do
                (backEvt', _) <- buttonAttr ("class" =: "button croe-has-margin-right") $ text "返回"
                -- 自己是创建人，并且任务状态是reviewing，才可以编辑
                let editable = user ^. user_id == task ^. task_creatorId && task ^. task_status == TaskStatusReviewing
                (editEvt', _) <- buttonAttr ("class" =: ("button croe-has-margin-right" <> if not editable then " is-hidden" else "")) $ text "编辑"
                let action = nextAction (task ^. task_status)
                    actionStr = showTaskAction action
                    hasAction = canDoAction action (user ^. user_id) task
                (actionEvt', _) <- buttonAttr ("class" =: ("button" <> if hasAction then "" else " is-hidden")) $ text actionStr
                pure (backEvt', editEvt')
          el "div" $ do
            postBuildEvt <- getPostBuild
            getTaskResult <- getTask (constDyn (Right taskId)) postBuildEvt
            let taskDetailEvt = filterRight (fmap reqResultToEither getTaskResult)
                descriptionEvt = fmap (^. taskDetail_description) taskDetailEvt
            descriptionDyn <- holdDyn "" descriptionEvt
            el "p" $ dynText descriptionDyn

          pure (backEvt, editEvt)
  where
    getTask = client ^. taskClient_get

showTaskStatus :: TaskStatus -> Text
showTaskStatus TaskStatusReviewing    = "草稿"
showTaskStatus TaskStatusPublished    = "公开"
showTaskStatus TaskStatusAccepted     = "已被接受"
showTaskStatus TaskStatusBeforeFinish = "即将完成"
showTaskStatus TaskStatusFinished     = "已完成"

showCreatorScore :: Maybe Double -> Text
showCreatorScore Nothing      = "无"
showCreatorScore (Just score) = T.pack $ printf "%.1f" (score / maxScore)
  where
    maxScore = 5

showDuration :: TimeZone -> (UTCTime, UTCTime) -> Text
showDuration tz duration = formatDuration localDuration
  where
    localDuration = durationToLocal tz duration

formatLocalTime :: LocalTime -> Text
formatLocalTime = T.pack . formatTime defaultTimeLocale "%m/%d %H:%M"

formatDuration :: (LocalTime, LocalTime) -> Text
formatDuration (begin, end) = formatLocalTime begin <> " ~ " <> formatLocalTime end

durationToLocal :: TimeZone -> (UTCTime, UTCTime) -> (LocalTime, LocalTime)
durationToLocal tz (t1, t2) = (utcToLocalTime tz t1, utcToLocalTime tz t2)

showTaskAction :: TaskAction -> Text
showTaskAction TaskActionPublish = "发布"
showTaskAction TaskActionAccept  = "接受"
showTaskAction TaskActionSubmit  = "提交"
showTaskAction TaskActionConfirm = "确认完成"

canDoAction :: TaskAction -> Int64 -> Task -> Bool
canDoAction TaskActionPublish myId task = task ^. task_creatorId == myId
canDoAction TaskActionAccept myId task  = task ^. task_creatorId /= myId
canDoAction TaskActionSubmit myId task  = task ^. task_takerId == Just myId
canDoAction TaskActionConfirm myId task = task ^. task_creatorId == myId
