{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CROE.Frontend.Widget.Entrance
  ( entranceWidget
  , loginWidget
  , registerWidget
  ) where

import           Data.Maybe                        (isJust)
import           Data.Text                         (Text)
import qualified Data.Text.Encoding                as T
import           Reflex.Dom                        hiding (button)
import           Reflex.Dom.Bulma.Component.Button
import           Reflex.Dom.Bulma.Component.Show   (hiddenClass, showWidget)
import           Servant.API

import           CROE.Common.User
import           CROE.Frontend.Client
import           CROE.Frontend.User
import           CROE.Frontend.Widget.Notification

entranceWidget :: (MonadWidget t m)
               => UserClient t m
               -> Dynamic t Text  -- ^route
               -> m (Dynamic t (Maybe UserPassword), Event t Text) -- update route
entranceWidget UserClient{..} routeDyn =
    elClass "div" "section" $
      divClass "container" $ do
        postBuild <- getPostBuild
        let isLogin = fmap (== "/login") routeDyn
            isRegister = fmap (== "/register") routeDyn
        (userPassword, registerEvt) <- showWidget isLogin (loginWidget _getProfile)
        showWidget isRegister registerWidget

        let updateRoute = leftmost
              [ fmap (const "/register") registerEvt
              , fmap (const "/login") postBuild
              ]

        pure (userPassword, updateRoute)

loginWidget :: forall t m. MonadWidget t m
            => GetProfile t m
            -> m (Dynamic t (Maybe UserPassword), Event t ()) -- ^(user state, register event)
loginWidget getProfile' =
    divClass "modal is-active" $ mdo
      divClass "modal-background" $ notification $ (def :: NotificationConfig t)
        & notificationConfig_show .~ errMsg'
      (userPassword', errMsg', registerClick') <- divClass "modal-content" $
        divClass "card" $ do
          divClass "card-header" $
            divClass "card-header-title" $ text "校园代办平台 - 登录"
          divClass "card-content" $ mdo
            emailInput <- divClass "field" $ do
              elClass "label" "label" $ text "邮箱"
              divClass "control has-icons-left has-icons-right" $ do
                let attrs = "class" =: "input"
                      <> "type" =: "text"
                      <> "placeholder" =: "校园邮箱"
                    modifyClass = (\valid -> if valid then "" else " is-danger") <$> updated isEmailValid
                    modifyAttrs = (\c -> "class" =: Just ("input" <> c)) <$> modifyClass
                emailInput' <- inputElement $ def & initialAttributes .~ attrs
                                                 & modifyAttributes .~ modifyAttrs
                elClass "span" "icon is-small is-left" $
                  elClass "i" "fas fa-envelope" blank
                elDynClass "span" (("icon is-small is-right" <>) <$> hiddenClass isEmailValid) $
                  elClass "i" "fas fa-times" blank
                elDynClass "span" (("icon is-small is-right" <>) <$> hiddenClass (fmap not isEmailValid)) $
                  elClass "i" "fas fa-check" blank
                pure emailInput'
            passwordInput <- divClass "field" $ do
              elClass "label" "label" $ text "密码"
              divClass "control has-icons-left has-icons-right" $ do
                let attrs = "class" =: "input"
                      <> "type" =: "password"
                      <> "placeholder" =: "密码"
                e <- inputElement $ def & initialAttributes .~ attrs
                elClass "span" "icon is-small is-left" $
                  elClass "i" "fas fa-key" blank
                elClass "span" "icon is-small is-right" $
                  elClass "i" "fas fa-check" blank
                pure e
            (loginClick, _) <- divClass "field" $ divClass "control is-expanded" $
              button ("class" =: "button is-link is-fullwidth is-rounded") $
                text "登录"
            (registerClick, _) <- divClass "field" $ divClass "control is-expanded" $
              button ("class" =: "button is-success is-fullwidth is-rounded") $
                text "注册或找回密码"

            -- controller
            let email = value emailInput
                isEmailValid = fmap (isJust . parseEmailAddress) email

                password = value passwordInput
            loginResult <- getProfile' (fmap Just (BasicAuthData <$> (T.encodeUtf8 <$> email) <*> (T.encodeUtf8 <$> password))) loginClick
            let resultEither = fmap reqResultToEither loginResult
                errMsg = filterLeft resultEither
            user <- holdDyn Nothing (fmap rightMay resultEither)
            let userPassword = (\u p -> UserPassword <$> u <*> pure p) <$> user <*> password

            pure (userPassword, errMsg, registerClick)
      pure (userPassword', registerClick')

registerWidget :: MonadWidget t m => m ()
registerWidget = text "register"

rightMay :: Either e a -> Maybe a
rightMay (Left _)  = Nothing
rightMay (Right x) = Just x
