{-# LANGUAGE RecursiveDo #-}

module CROE.Frontend.Widget.TaskList.ViewTask
  ( viewTaskWidget
  , showTaskStatus
  , showCreatorScore
  ) where

import           Control.Lens
import           Control.Monad.IO.Class
import           CROE.Common.Task
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
               => (Int64, Task)
               -> TaskClient t m
               -> m (Event t (), Event t ()) -- ^(go back, update task)
viewTaskWidget (taskId, task) client =
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
                (editEvt', _) <- buttonAttr ("class" =: "button croe-has-margin-right") $ text "编辑"
                (publishEvt', _) <- buttonAttr ("class" =: "button") $ text "发布"
                pure (backEvt', editEvt')
          el "div" $
            el "p" $ text "详细描述"

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
