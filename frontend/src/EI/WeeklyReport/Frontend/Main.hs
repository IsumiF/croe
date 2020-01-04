{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EI.WeeklyReport.Frontend.Main
  ( main
  ) where

import           Control.Concurrent             (threadDelay)
import           Control.Lens
import           Control.Monad.IO.Class
import           Data.Functor                   (void)
import           Data.Maybe                     (fromJust)
import           Data.Proxy                     (Proxy (..))
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Data.Time
import           JSDOM                          (currentDocumentUnchecked)
import           JSDOM.Element                  (Element, setInnerHTML)
import           JSDOM.NonElementParentNode     (getElementById)
import           Language.Javascript.JSaddle    (JSM, ghcjsPure, isUndefined,
                                                 js, js1, jsg, jsg0, liftJSM)
import           Reflex.Dom                     hiding (Element,
                                                 mainWidgetWithHead)
import           Reflex.Dom.Main                (mainWidgetWithHead)
import           Servant.Reflex

import           EI.WeeklyReport.Common.API
import           FrontendUtils.Component.Button
import           FrontendUtils.JSM              (JsmConfig (..),
                                                 loadAndWaitScripts, runJsm)

main :: IO ()
-- TODO load port and root dir dynamically
main = runJsm (WarpConfig 8080 "static") mainJsm

mainJsm :: JSM ()
mainJsm = do
    loadAndWaitScripts
      [ ("js_main.js", "jsMain")
      , ("https://unpkg.com/material-components-web@4.0.0/dist/material-components-web.js", "mdc")
      ]
    _ <- jsg0 ("jsMain" :: Text)

    mainWidgetWithHead headElem bodyElem

headElem :: MonadWidget t m
         => m ()
headElem = do
    elAttr "link" ("rel" =: "stylesheet" <> "href" =: "https://unpkg.com/material-components-web@4.0.0/dist/material-components-web.css") blank
    elAttr "link" ("rel" =: "stylesheet" <> "href" =: "https://fonts.googleapis.com/icon?family=Material+Icons") blank
    el "style" $ text "body { margin: 0; }"

bodyElem :: MonadWidget t m
         => m ()
bodyElem = do
    submit <- initClientFunctions

    primaryElem submit

primaryElem :: MonadWidget t m
            => FuncSubmit t m
            -> m ()
primaryElem submit = do
    tz <- liftIO getCurrentTimeZone
    defaultEndTime <- liftIO getCurrentTime
    let defaultBeginTime = addUTCTime (-7 * 24 * 60 * 60) defaultEndTime
        defaultLocalBeginTime = utcToLocalTime tz defaultBeginTime
        defaultLocalEndTime = utcToLocalTime tz defaultEndTime

    topAppBar

    elAttr "div" ("style" =: "width: 100%; text-align: center;" <> "class" =: "mdc-top-app-bar--fixed-adjust") $
      elAttr "div" ("style" =: "display: inline-block;") $
        elAttr "form" ("style" =: "display: flex; flex-direction: column;") $ do
          (projectIdDyn, bizIdDyn, usernameDyn) <- elAttr "div" ("style" =: "margin-top: 1em;") $ do
            projectIdDyn <- mdcTextInput "project-id" "产品线ID" "504" "number"
            bizIdDyn <- mdcTextInput "biz-id" "空间ID" "10016" "number"
            usernameDyn <- mdcTextInput "username" "邮箱前缀" "fengzelin.isumi" "text"
            pure (projectIdDyn, bizIdDyn, usernameDyn)
          (startTimeDyn, endTimeDyn) <- elAttr "div" ("style" =: "margin-top: 1em;") $ do
            startTimeDyn <- mdcTextInput "start-time" "起始时间" (formatTextInputDateTime defaultLocalBeginTime) "datetime-local"
            endTimeDyn <- mdcTextInput "end-time" "结束时间" (formatTextInputDateTime defaultLocalEndTime) "datetime-local"
            pure (startTimeDyn, endTimeDyn)
          submitEvt <- elAttr "div" ("style" =: "text-align: center; margin-top: 1em;") submitButton
          let paramsDyn = formToGenerateReportParams <$> projectIdDyn
                                                     <*> bizIdDyn
                                                     <*> usernameDyn
                                                     <*> startTimeDyn
                                                     <*> endTimeDyn
                                                     <*> constDyn tz
          resultEvt <- submit (fmap Right paramsDyn) submitEvt
          outputDyn <- holdDyn "" (fmap resultToText resultEvt)
          writeToClipboard (updated outputDyn)
          htmlOutput outputDyn

topAppBar :: MonadWidget t m
          => m ()
topAppBar = do
    elAttr "header" ("class" =: "mdc-top-app-bar mdc-top-app-bar--fixed" <> "id" =: elemId) $
      divClass "mdc-top-app-bar__row" $
        elClass "section" "mdc-top-app-bar__section mdc-top-app-bar__section--align-start" $
          elClass "span" "mdc-top-app-bar__title" $ text "周报生成器"
    postBuildEvt <- getPostBuildWithElemId elemId
    performEvent_ $ ffor postBuildEvt $ \e' ->
      void $ liftJSM $ jsg ("mdc" :: Text) ^. (js ("topAppBar" :: Text) .
        js ("MDCTopAppBar" :: Text) . js1 ("attachTo" :: Text) e')
  where
    elemId = "top-app-bar"

mdcTextInput :: MonadWidget t m
             => Text -- ^id
             -> Text -- ^label
             -> Text -- ^initial value
             -> Text -- ^type
             -> m (Dynamic t Text) -- ^user input
mdcTextInput elemId label initialValue inputType =
    elAttr "div" ("id" =: elemId <> "class" =: "mdc-text-field imdc-to-init" <> "data-imdc" =: "TextField") $ do
      ti <- textInput $ def & textInputConfig_inputType .~ inputType
                            & textInputConfig_initialValue .~ initialValue
                            & textInputConfig_attributes .~ constDyn tiAttrs
      divClass "mdc-line-ripple" blank
      elAttr "label" ("for" =: textInputId <> "class" =: "mdc-floating-label") $
        text label
      postBuildEvt <- getPostBuildDelay
      performEvent_ $ ffor postBuildEvt $ \_ -> do
        doc <- currentDocumentUnchecked
        e <- getElementById doc elemId
        void $ liftJSM $ jsg ("mdc" :: Text) ^. (js ("textField" :: Text) . js ("MDCTextField" :: Text) . js1 ("attachTo" :: Text) e)
      pure $ ti ^. textInput_value
  where
    textInputId = elemId <> "_textInput"
    tiAttrs = "id" =: textInputId <> "class" =: "mdc-text-field__input"

submitButton :: MonadWidget t m
             => m (Event t ())
submitButton = do
    (e, _) <- buttonAttr ("id" =: "submit-button" <> "class" =: "mdc-button mdc-button--raised imdc-to-init"
      <> "type" =: "button" <> "data-imdc" =: "RippleButton") $ do
      elAttr "span" ("class" =: "mdc-button__ripple") blank
      text "提交"
    pure e

htmlOutput :: MonadWidget t m
           => Dynamic t Text
           -> m ()
htmlOutput t = do
    performEvent_ $ ffor (updated t) $ \output -> do
      document <- currentDocumentUnchecked
      e <- getElementById document elemId
      let e' = fromJust e
      liftJSM $ setInnerHTML e' output
    elAttr "div" ("id" =: elemId <> "style" =: "text-align: left;") blank
  where
    elemId = "html-output" :: Text

writeToClipboard :: MonadWidget t m
                 => Event t Text
                 -> m ()
writeToClipboard e =
    performEvent_ $ ffor e $ \t ->
      void $ liftJSM $ jsg ("navigator" :: Text) ^.
        (js ("clipboard" :: Text) . js1 ("writeText" :: Text) t)

getPostBuildDelay :: MonadWidget t m
                  => m (Event t ())
getPostBuildDelay = getPostBuild >>= delay 0.3

getPostBuildWithElemId :: MonadWidget t m
                       => Text -- ^id
                       -> m (Event t Element)
getPostBuildWithElemId elemId =
    getPostBuildWait $ do
      mdc <- jsg ("mdc" :: Text)
      isMdcNull <- ghcjsPure $ isUndefined mdc
      if isMdcNull
      then pure Nothing
      else do
        doc <- currentDocumentUnchecked
        getElementById doc elemId

getPostBuildWait :: MonadWidget t m
                 => JSM (Maybe a)
                 -> m (Event t a)
getPostBuildWait m = do
    evt <- getPostBuild
    performEventAsync $ ffor evt $ \_ tellResult -> liftJSM $
      forkJSM $ do
        e <- retryAction m
        liftIO $ tellResult e

retryAction :: JSM (Maybe a) -> JSM a
retryAction m = do
    a <- m
    case a of
      Just e -> pure e
      Nothing -> do
        liftIO $ threadDelay (100 * 1000)
        retryAction m

resultToText :: ReqResult () Text -> Text
resultToText (ResponseSuccess _ t _) = t
resultToText (ResponseFailure _ t _) = t
resultToText (RequestFailure _ t)    = t

formatTextInputDateTime :: FormatTime t => t -> Text
formatTextInputDateTime = T.pack . formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M"))

formToGenerateReportParams :: Text -- ^projectId
                           -> Text -- ^bizId
                           -> Text -- ^username
                           -> Text -- ^startTime
                           -> Text -- ^endTime
                           -> TimeZone -- ^local time zone
                           -> GenerateReportParams
formToGenerateReportParams projectId bizId username startTime endTime tz =
    GenerateReportParams projectId' bizId' username startTime' endTime'
  where
    projectId' = read (T.unpack projectId) :: Int
    bizId' = read (T.unpack bizId) :: Int
    startTime' = localTimeToUTC tz . parseFormTime $ startTime
    endTime' = localTimeToUTC tz . parseFormTime $ endTime

parseFormTime :: Text -> LocalTime
parseFormTime = fromJust . parseTimeM True defaultTimeLocale (iso8601DateFormat (Just "%H:%M")) . T.unpack

initClientFunctions :: forall t m. MonadWidget t m
                    => m (FuncSubmit t m)
initClientFunctions = do
    let submitWeeklyReport = client (Proxy :: Proxy API) (Proxy :: Proxy m) (Proxy :: Proxy ())
          (constDyn baseUrl)
    pure submitWeeklyReport

type FuncSubmit t m = Dynamic t (Either Text GenerateReportParams)
                   -> Event t ()
                   -> m (Event t (ReqResult () Text))

baseUrl :: BaseUrl
#ifdef ghcjs_HOST_OS
baseUrl = BasePath "/"
#else
baseUrl = BaseFullUrl Http "127.0.0.1" 8081 "/"
#endif
