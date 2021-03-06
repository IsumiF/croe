{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CROE.Frontend.Widget.TaskList
  ( taskListWidget
  ) where

import           Control.Lens
import           Control.Monad                          (join)
import           Control.Monad.IO.Class
import           Data.Bifunctor                         (first)
import           Data.Foldable                          (forM_)
import           Data.Functor                           (void)
import           Data.Int
import           Data.List.Split                        (chunksOf)
import           Data.Maybe
import           Data.Text                              (Text)
import qualified Data.Text                              as T
import           Data.Time
import           Data.Traversable                       (forM)
import           Data.Word
import           Reflex.Dom
import           Reflex.Dom.Bulma.Component.Show        (showWidget)
import           Reflex.Dom.Bulma.Util                  (parseFormDateTime,
                                                         renderFormDateTime)
import           Servant.API

import           CROE.Common.API.Task
import           CROE.Common.School
import           CROE.Common.User
import           CROE.Common.Util                       (readt, readtMaybe,
                                                         safeHead, showt)
import           CROE.Frontend.Client
import           CROE.Frontend.Widget.Navbar            (navbarWidget)
import           CROE.Frontend.Widget.Pagination
import           CROE.Frontend.Widget.TaskList.ViewTask
import           Reflex.Dom.Bulma.Component.Button

pageSize :: Integral a => a
pageSize = 8

rowWidth :: Integral a => a
rowWidth = 4

taskListWidget :: forall t m. MonadWidget t m
               => User
               -> ProtectedClient t m
               -> m ()
taskListWidget user protectedClient = mdo
    navbarWidget
    (newTaskEvt, viewTaskEvt) <- showWidget isViewListDyn $ elClass "section" "section" $
      divClass "container" $
        divClass "tile is-ancestor" $
          divClass "tile is-vertical" $ mdo
            (newTaskEvt', _) <- divClass "tile is-parent" $
              divClass "tile is-child" $
                buttonAttr ("class" =: "button is-link") $ text "发布新任务"
            (queryTextEvt, myPublishedDyn, myAcceptedDyn) <- divClass "tile is-parent" $
              divClass "tile is-child is-12 box" $
                divClass "field is-grouped" $ mdo
                  queryTextDyn' <- divClass "control is-expanded" $ do
                    ti <- inputElement $ def
                      & initialAttributes .~ ("class" =: "input" <> "type" =: "text" <> "placeholder" =: "智能搜索")
                    pure (value ti)
                  (e, _) <- divClass "control" $
                    buttonAttr ("class" =: "button is-primary") $ text "搜索"
                  checkboxMyPublished <- inputElement $ def
                    & initialAttributes .~ ( "class" =: "is-checkradio"
                                           <> "id" =: "checkradio-my-published"
                                           <> "type" =: "checkbox"
                                           )
                    & inputElementConfig_setChecked .~ fmapMaybe (\p -> if p then Just False else Nothing) myAcceptedCheckedChange
                  let myPublishedCheckedChange = _inputElement_checkedChange checkboxMyPublished
                  elAttr "label" ("for" =: "checkradio-my-published") $
                    text "我发布的"
                  checkboxMyAccepted <- inputElement $ def
                    & initialAttributes .~ ( "class" =: "is-checkradio"
                                           <> "id" =: "checkradio-my-accepted"
                                           <> "type" =: "checkbox"
                                           )
                    & inputElementConfig_setChecked .~ fmapMaybe (\p -> if p then Just False else Nothing) myPublishedCheckedChange
                  let myAcceptedCheckedChange = _inputElement_checkedChange checkboxMyAccepted
                      myPublished = _inputElement_checked checkboxMyPublished
                      myAccepted = _inputElement_checked checkboxMyAccepted
                  elAttr "label" ("for" =: "checkradio-my-accepted") $
                    text "我接受的"
                  pure (tagPromptlyDyn queryTextDyn' e, myPublished, myAccepted)
            queryTextDyn <- holdDyn "" queryTextEvt
            let queryConditionDyn = makeTaskQueryCondition
                  <$> queryTextDyn
                  <*> fmap (\p -> if p then Just userId else Nothing) myPublishedDyn
                  <*> fmap (\p -> if p then Just userId else Nothing) myAcceptedDyn
                  <*> offsetDyn
                triggerSearchEvt = leftmost
                  [ void (updated queryConditionDyn)
                  , void (updated myPublishedDyn)
                  , void (updated myAcceptedDyn)
                  ]
            searchReqResult <- searchTask (fmap Right queryConditionDyn) triggerSearchEvt
            let searchResult = filterRight (fmap reqResultToEither searchReqResult)
                totalEvt = fmap (^. taskSearchResult_total) searchResult
                tasksEvt :: Event t [(Int64, Task)] =
                  fmap (\r -> fmap (first readt) (r ^. taskSearchResult_tasks)) searchResult
            viewTaskEvtDyn <- widgetHold (pure never) $ ffor tasksEvt $ \tasks -> do
              cardClickEvts <- forM (chunksOf rowWidth tasks) $ \row ->
                divClass "tile" $
                  forM row $ \(taskId, task) -> do
                    e <- taskCard task
                    pure (fmap (const (taskId, task)) e)
              pure $ leftmost (fmap leftmost cardClickEvts)
            totalDyn <- holdDyn 1 totalEvt
            pg <- divClass "tile is-parent tasklist-pagination-tile" $ divClass "tile is-child has-text-right" $
              pagination $ (def :: PaginationConfig t)
                & paginationConfig_total .~ fmap (`divUpper` pageSize) totalDyn
            let currentPageDyn = pg ^. pagination_current
                offsetDyn = fmap (\x -> (x - 1) * pageSize) currentPageDyn
                viewTaskEvt' = switchDyn viewTaskEvtDyn
            pure (newTaskEvt', viewTaskEvt')

    resultDyn <- widgetHold (pure (never, never)) $ ffor taskOpEvt $ \taskOp ->
      case taskOp of
        Nothing      -> pure (never, never)
        Just taskOp' -> handleTaskOperation taskOp' user schoolClient taskClient
    let backEvt = switchDyn (fmap fst resultDyn)
        resultOpEvt = switchDyn (fmap snd resultDyn)
        taskOpEvt :: Event t (Maybe TaskOperation) =
          leftmost [ fmap (Just . const NewTask) newTaskEvt
                   , fmap (Just . ViewTask) viewTaskEvt
                   , fmap (const Nothing) backEvt
                   , fmap Just resultOpEvt
                   ]
    taskOpDyn <- holdDyn Nothing taskOpEvt
    let isViewListDyn = fmap isNothing taskOpDyn
    blank
  where
    userId = user ^. user_id
    schoolClient = protectedClient ^. protectedClient_school
    taskClient = protectedClient ^. protectedClient_task
    searchTask = taskClient ^. taskClient_search

divUpper :: Integral a => a -> a -> a
divUpper x y =
    if x `mod` y == 0
    then x `div` y
    else x `div` y + 1

makeTaskQueryCondition :: Text -- ^query string
                       -> Maybe Int64 -- ^creator id
                       -> Maybe Int64 -- ^taker id
                       -> Integer -- ^offset
                       -> TaskQueryCondition
makeTaskQueryCondition queryText creatorId takerId offset =
    def & taskQueryCondition_query .~ queryText
        & taskQueryCondition_creatorId .~ creatorId
        & taskQueryCondition_takerId .~ takerId
        & taskQueryCondition_limit .~ defaultLimit
        & taskQueryCondition_offset .~ offset
  where
    defaultLimit = 8

data TaskOperation = NewTask
                   | UpdateTask (Int64, Task)
                   | ViewTask (Int64, Task)
                     deriving (Show, Eq)

taskCard :: MonadWidget t m
         => Task -- ^task with id
         -> m (Event t ()) -- ^returns click event
taskCard task = do
    (raw, _) <- elClass' "div" "tile is-parent is-3" $
      divClass "tile is-child box" $ do
        elClass "h4" "title is-4" $ text (task ^. task_title)
        elClass "p" "is-size-6" $ text (task ^. task_abstract)
        elAttr "div" ("class" =: "taskcard-topmost-field") $
          elClass "span" "tag is-medium is-link" $
            text $ "发布人：" <> task ^. task_creatorName
        elAttr "nav" ("class" =: "level taskcard-other-field") $ do
          divClass "level-left" $
            divClass "level-item" $
              elClass "span" "tag is-medium is-success" $
                text $ "奖金：" <> showt (task ^. task_reward)
          divClass "level-rigit" $
            divClass "level-item" $
              elClass "span" "tag is-medium is-info" $
                text $ "评分：" <> showCreatorScore (task ^. task_creatorScore)
        divClass "taskcard-other-field" $
          elClass "span" "tag is-medium is-light" $ do
            tz <- liftIO getCurrentTimeZone
            text $ formatDuration (durationToLocal tz (task ^. task_duration))
        divClass "taskcard-other-field" $
          elClass "span" "tag is-medium is-light" $
            text $ task ^. task_location
    pure $ domEvent Click raw
  where
    formatLocalTime :: LocalTime -> Text
    formatLocalTime = T.pack . formatTime defaultTimeLocale "%m/%d %H:%M"

    formatDuration :: (LocalTime, LocalTime) -> Text
    formatDuration (begin, end) = formatLocalTime begin <> " ~ " <> formatLocalTime end

durationToLocal :: TimeZone -> (UTCTime, UTCTime) -> (LocalTime, LocalTime)
durationToLocal tz (t1, t2) = (utcToLocalTime tz t1, utcToLocalTime tz t2)

handleTaskOperation :: MonadWidget t m
                    => TaskOperation
                    -> User
                    -> SchoolClient t m
                    -> TaskClient t m
                    -> m (Event t (), Event t TaskOperation) -- ^event to go back, event to generate another operation
handleTaskOperation NewTask _ schoolClient taskClient = do
    e <- putTaskWidget Nothing schoolClient taskClient
    pure (e, never)
handleTaskOperation (UpdateTask taskId) _ schoolClient taskClient = do
    e <- putTaskWidget (Just taskId) schoolClient taskClient
    pure (e, never)
handleTaskOperation (ViewTask taskEntity) user _ taskClient = do
    (back, updateTask) <- viewTaskWidget user taskEntity taskClient
    pure (back, fmap (const (UpdateTask taskEntity)) updateTask)

putTaskWidget :: forall t m. MonadWidget t m
              => Maybe (Int64, Task) -- ^initial task id
              -> SchoolClient t m
              -> TaskClient t m
              -> m (Event t ())
putTaskWidget initialTaskEntity schoolClient taskClient =
    elClass "section" "section" $
      divClass "container" $ mdo
        tz <- liftIO getCurrentTimeZone
        let initialTask = fmap snd initialTaskEntity

        titleDyn <- divClass "field is-horizontal" $ do
          divClass "field-label is-normal" $
            elClass "label" "label" $ text "标题"
          divClass "field-body" $
            divClass "field" $
              elClass "p" "control" $ do
                ti <- inputElement $ def
                  & initialAttributes .~ ("class" =: "input" <> "type" =: "text")
                  & inputElementConfig_initialValue .~ maybe "" (^. task_title) initialTask
                pure (value ti)
        abstractDyn <- divClass "field is-horizontal" $ do
          divClass "field-label is-normal" $
            elClass "label" "label" $ text "摘要"
          divClass "field-body" $
            divClass "field" $
              elClass "p" "control" $ do
                ti <- textAreaElement $ def
                  & initialAttributes .~ ("class" =: "textarea" <> "rows" =: "3")
                  & textAreaElementConfig_initialValue .~ maybe "" (^. task_abstract) initialTask
                pure (value ti)
        rewardDyn :: Dynamic t Word64 <- divClass "field is-horizontal" $ do
          divClass "field-label is-normal" $
            elClass "label" "label" $ text "赏金"
          divClass "field-body" $
            divClass "field" $
              elClass "p" "control" $ do
                ti <- inputElement $ def
                  & initialAttributes .~ ("class" =: "input" <> "type" =: "number" <> "value" =: "100" <> "step" =: "1")
                  & inputElementConfig_initialValue .~ showt (maybe 100 (^. task_reward) initialTask)
                pure $ fmap (read . T.unpack) (value ti)
        (beginTimeDyn, endTimeDyn) <- divClass "field is-horizontal" $ do
          divClass "field-label is-normal" $
            elClass "label" "label" $ text "时间区间"
          divClass "field-body" $
            divClass "field is-grouped" $ do
              now <- liftIO getCurrentTime
              let !nowLocal = utcToLocalTime tz now
                  !tomorrowLocal = nowLocal {localDay = addDays 1 (localDay nowLocal)}
                  (initialTimeFrom, initialTimeTo) =
                    maybe (nowLocal, tomorrowLocal)
                      (durationToLocal tz . (^. task_duration)) initialTask
              beginTimeDyn' <- elClass "p" "control" $ do
                ti <- inputElement $ def
                        & initialAttributes .~ ("class" =: "input"
                           <> "type" =: "datetime-local"
                           )
                        & inputElementConfig_initialValue .~ renderFormDateTime initialTimeFrom
                pure $ fmap parseFormDateTime (value ti)
              endTimeDyn' <- elClass "p" "control" $ do
                ti <- inputElement $ def
                        & initialAttributes .~ ("class" =: "input"
                           <> "type" =: "datetime-local"
                           )
                        & inputElementConfig_initialValue .~ renderFormDateTime initialTimeTo
                pure $ fmap parseFormDateTime (value ti)
              pure (beginTimeDyn', endTimeDyn')
        campusIdDyn <- divClass "field is-horizontal" $ do
          divClass "field-label is-normal" $
            elClass "label" "label" $ text "学校、校区"
          divClass "field-body" $
            divClass "field is-grouped" $ do
              postBuild <- getPostBuild
              schoolsReqResult <- getSchool postBuild
              let schoolListEvt = filterRight (fmap reqResultToEither schoolsReqResult)
              (schoolSelect, _) <- elClass "p" "control" $
                divClass "select" $ do
                  let config = def & selectElementConfig_initialValue .~ "0"
                  selectElement config $
                    widgetHold_ blank $ ffor schoolListEvt $ \schools -> do
                      forM_ (zip ([0..] :: [Int]) schools) $ \(idx, sc) ->
                        elAttr "option" ("value" =: showt idx) $ text (_school_name sc)
                      blank
              schoolListDyn <- holdDyn [] schoolListEvt
              let schoolIdxDyn :: Dynamic t Int =
                    fromMaybe 0 . readtMaybe <$> _selectElement_value schoolSelect
                  campusRefreshEvt = leftmost [void (updated schoolIdxDyn), void schoolListEvt]
              campusSelectedValue <- elClass "p" "control" $
                divClass "select" $ do
                  d <- widgetHold (pure (constDyn "0")) $ ffor (tagPromptlyDyn ((,) <$> schoolIdxDyn <*> schoolListDyn) campusRefreshEvt) $ \(idx, schools) -> do
                    let !school = schools !! idx
                        !campusList = school ^. school_campusList
                        !initialValue = case safeHead campusList of
                          Nothing        -> 0
                          Just firstItem -> firstItem ^. schoolCampus_id
                        !config = def & selectElementConfig_initialValue .~ showt initialValue
                    (s, _) <- selectElement config $
                      forM_ campusList $ \campus ->
                        elAttr "option" ("value" =: showt (campus ^. schoolCampus_id)) $
                          text (campus ^. schoolCampus_name)
                    pure $ _selectElement_value s
                  pure $ join d
              let campusIdDyn' :: Dynamic t Int64 = fromMaybe 0 . readtMaybe <$> campusSelectedValue
              pure campusIdDyn'
        descriptionDyn <- divClass "field is-horizontal" $ do
          divClass "field-label is-normal" $
            elClass "label" "label" $ text "详细描述"
          divClass "field-body" $
            divClass "field" $
              elClass "p" "control" $ do
                ti <- textAreaElement $ def
                  & initialAttributes .~ ("class" =: "textarea" <> "rows" =: "15")
                  & textAreaElementConfig_setValue .~ initialDescription
                pure (value ti)
        (backEvt, submitEvt) <- elClass "nav" "level" $ do
          divClass "level-left" blank
          divClass "level-right" $ do
            (backEvt', _) <- divClass "level-item" $
              buttonAttr ("class" =: "button is-light") $ text "返回"
            (submitEvt', _) <- divClass "level-item" $
              buttonAttr ("class" =: "button is-primary") $ text "提交"
            pure (backEvt', submitEvt')

        postBuildEvt <- getPostBuild
        initialDescription <- case initialTaskEntity of
          Just (taskId, _) -> do
            reqResult <- getTask (constDyn (Right taskId)) postBuildEvt
            let x = fmap (either (const "") (^. taskDetail_description) . reqResultToEither) reqResult
            pure x
          Nothing -> pure (fmap (const "") postBuildEvt)
        let putTask = case initialTaskEntity of
              Nothing     -> \a b -> do
                resultEvt <- newTask a b
                pure ((fmap . fmap) (const NoContent) resultEvt)
              Just (taskId, _) -> updateTask (constDyn . Right $ taskId)
            durationDyn = (,) <$> fmap (localTimeToUTC tz) beginTimeDyn
                              <*> fmap (localTimeToUTC tz) endTimeDyn
            newTaskRequest = NewTaskRequest
              <$> titleDyn
              <*> rewardDyn
              <*> campusIdDyn
              <*> durationDyn
              <*> abstractDyn
              <*> descriptionDyn
        submittedEvt <- putTask (fmap Right newTaskRequest) submitEvt

        pure $ leftmost [backEvt, void submittedEvt]
    where
      getTask = taskClient ^. taskClient_get
      updateTask = taskClient ^. taskClient_update
      newTask = taskClient ^. taskClient_new
      getSchool = schoolClient ^. schoolClient_get
