{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CROE.Frontend.Widget.Chat
  ( chatWidget
  ) where

import           Control.Lens
import           Control.Monad.IO.Class
import           CROE.Common.API.WithTotal
import           CROE.Common.Chat
import           CROE.Common.User
import           CROE.Common.Util                  (divUpper, showt)
import           CROE.Common.WebSocket
import           CROE.Frontend.Client              (reqResultToEither)
import           CROE.Frontend.Client.Protected    (ChatClient (..),
                                                    chatClient_authData,
                                                    chatClient_baseUrl)
import           CROE.Frontend.Widget.Pagination
import           Data.Functor                      (void)
import           Data.Map.Strict                   (Map)
import           Data.String.Interpolate           (i)
import           Data.Text                         (Text)
import qualified Data.Text                         as T
import qualified Data.Text.Encoding                as T
import           Data.Witherable
import           Network.HTTP.Types                (urlEncode)
import           Reflex.Dom
import           Reflex.Dom.Bulma.Component.Button
import           Reflex.Dom.Contrib.CssClass
import           Servant.API                       (BasicAuthData (..))
import           Servant.Common.BaseUrl
import           Servant.Common.Req

chatWidget :: forall t m. MonadWidget t m
           => ChatClient t m
           -> m ()
chatWidget chatClient@ChatClient{..} =
    divClass "chatbox-wrapper" $ mdo
      sendMessageEvt <- elDynClass "div" (addConstClass (mkIsHidden chatboxCardVisible) "chatbox card") $
        divClass "card-content" $ divClass "columns" $ do
          divClass "column contact-list is-narrow" $ mdo
            -- contact list
            divClass "menu" $ elClass "ul" "menu-list" $ do
              blank

            -- pagination
            pg <- el "div" $ pagination $ (def :: PaginationConfig t)
                    & paginationConfig_size .~ PgSizeIsSmall
                    & paginationConfig_total .~ fmap (fromIntegral . (`divUpper` pgSize)) totalDyn
            let currentPageDyn = pg ^. pagination_current
                pgSize = 6
                limitDyn = constDyn (QParamSome pgSize)
                offsetDyn = fmap (\x -> QParamSome $ (fromIntegral x - 1) * pgSize) currentPageDyn
            postBuildEvt <- getPostBuild
            let refreshContactList = leftmost [postBuildEvt, void (updated currentPageDyn)]
            contactsReqResult <- _chatClient_contactList limitDyn offsetDyn refreshContactList
            let contactsPageEvt = filterRight (fmap reqResultToEither contactsReqResult)
                totalEvt = fmap (^. withTotal_total) contactsPageEvt
                contactsEvt = fmap (^. withTotal_data) contactsPageEvt
            performEvent_ $ ffor contactsEvt $ \cs -> liftIO (print cs)
            totalDyn <- holdDyn 1 (traceEvent "total" totalEvt)

            pure ()
          divClass "is-divider-vertical" blank
          sendMsgBodyEvt <- divClass "chatbox-message-area column" $ do
            el "div" $ do
              (showMoreEvt, _) <- divClass "chatbox-show-more-message" $ buttonAttr ("class" =: "button is-small") $
                text "显示更多"
              pure ()
            msgInput <- el "div" $ inputElement $ def
              & initialAttributes .~ ("class" =: "input" <> "type" =: "text")
            let msgPressEnterEvt :: Event t () = fmapMaybe (\c -> if c == 13 then Just () else Nothing) $
                  domEvent Keypress msgInput
                (msgDyn :: Dynamic t Text) = value msgInput
            pure $ tagPromptlyDyn msgDyn msgPressEnterEvt
          pure $ attachPromptlyDynWith SendChatMessage (constDyn 2) sendMsgBodyEvt
      (clickChatbox, _) <- buttonAttr ("class" =: "button is-info is-pulled-right") $ do
        el "p" $ text "聊天室"
        elClass "span" "icon is-right" $ do
          elDynClass "i" (addConstClass (mkIsHidden (fmap not chatboxCardVisible)) "fas fa-caret-up") blank
          elDynClass "i" (addConstClass (mkIsHidden chatboxCardVisible) "fas fa-caret-down") blank

      chatboxCardVisible <- toggle False clickChatbox

      receiveMessageEvt <- realtimeMessaging chatClient sendMessageEvt
      performEvent_ $ ffor receiveMessageEvt $ \msg ->
        liftIO $ print msg
      pure ()

contactItem :: forall t m. MonadWidget t m
            => Contact
            -> Dynamic t Bool -- ^is active
            -> m ()
contactItem (Contact user unreadCount) isActiveDyn =
    el "li" $ elDynAttr "a" attrDyn $
      text [i|#{user ^. user_name}(#{user ^. user_email})|]
  where
    activeClass :: Dynamic t CssClass
    activeClass = fmap (\p -> if p then "is-active" else mempty) isActiveDyn

    attrDyn = addToClassAttr <$> activeClass <*> pure (badgeAttr unreadCount)

-- list item example
{- <li><a class="is-active has-badge-rounded has-badge-danger has-badge-medium"
        data-badge="3">fzl(fengzlin@mail2.sysu.edu.cn)</a></li>
-}

badgeAttr :: (Integral i, Show i)
          => i
          -> Map Text Text
badgeAttr 0 = mempty
badgeAttr n =
    "class" =: "has-badge-rounded has-badge-danger has-badge-medium"
    <> "data-badge" =: showt n

realtimeMessaging :: forall t m. MonadWidget t m
                  => ChatClient t m
                  -> Event t SendChatMessage
                  -> m (Event t ReceiveChatMessage)
realtimeMessaging chatClient sendEvt = do
    let config :: WebSocketConfig t WsMessage =
          def & webSocketConfig_send .~ fmap ((: []) . WsSendChatMessage) sendEvt
              & webSocketConfig_reconnect .~ True
    wrapped <- dyn $ ffor authDataDyn $ \authDataMaybe ->
      case authDataMaybe of
        Nothing -> pure never
        Just authData -> do
          ws :: RawWebSocket t (Maybe WsMessage) <- jsonWebSocket (mkWsUrl authData baseUrl) config
          pure (ws ^. webSocket_recv)
    recvMaybeEvt <- switchHold never wrapped
    let recvEvt = mapMaybe (preview _WsReceiveChatMessage) . catMaybes $ recvMaybeEvt
    pure recvEvt
  where
    authDataDyn = chatClient ^. chatClient_authData
    baseUrl = chatClient ^. chatClient_baseUrl

mkWsUrl :: BasicAuthData -> BaseUrl -> Text
mkWsUrl _ (BasePath _) = error "base path is not supported"
mkWsUrl authData (BaseFullUrl _ host port _) = url
  where
    authDataStr = toUrlAuthStr authData
    url = [i|ws://#{authDataStr}@#{host}:#{port}/ws|]

toUrlAuthStr :: BasicAuthData -> Text
toUrlAuthStr (BasicAuthData username password) = T.decodeUtf8 $ urlEncode False $
    username <> ":" <> password

mkIsHidden :: Reflex t
           => Dynamic t Bool -- ^is visible
           -> Dynamic t CssClass
mkIsHidden = fmap (\p -> if p then mempty else "is-hidden")

addConstClass :: Reflex t
              => Dynamic t CssClass
              -> Text
              -> Dynamic t Text
addConstClass d c = fmap (renderClass . (<> constClass)) d
  where
    constClass = manyClasses (T.words c)
