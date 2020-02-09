{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CROE.Frontend.Widget.Entrance
  ( entranceWidget
  , loginWidget
  , registerWidget
  ) where

import           Data.Functor                      (void)
import           Data.Maybe                        (isJust)
import           Data.Text                         (Text)
import qualified Data.Text.Encoding                as T
import           Reflex.Dom
import           Reflex.Dom.Bulma.Component.Button
import           Reflex.Dom.Bulma.Component.Show   (hiddenClass, showWidget)
import           Servant.API
import           Servant.Reflex

import           CROE.Common.API.User
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
        (userPasswordLogin, registerEvt) <- showWidget isLogin (loginWidget _getProfile)
        userPasswordRegister <- showWidget isRegister (registerWidget UserClient{..})
        let userPassword = zipDynWith (\u1 u2 -> if isJust u1 then u1 else u2) userPasswordLogin userPasswordRegister

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
              buttonAttr ("class" =: "button is-link is-fullwidth is-rounded") $
                text "登录"
            (registerClick, _) <- divClass "field" $ divClass "control is-expanded" $
              buttonAttr ("class" =: "button is-success is-fullwidth is-rounded") $
                text "注册或找回密码"

            -- controller
            let email = value emailInput
                isEmailValid = fmap (isJust . parseEmailAddress) email
                password = value passwordInput
            loginResult <- getProfile' (fmap Just (BasicAuthData <$> (T.encodeUtf8 <$> email) <*> (T.encodeUtf8 <$> password))) loginClick
            let resultEither = fmap reqResultToEither loginResult
                errMsg = "用户名或密码错误" <$ filterLeft resultEither
            user <- holdDyn Nothing (fmap rightMay resultEither)
            passwordOnUserChange <- holdDyn "" (tagPromptlyDyn password (updated user))
            let userPassword = (\u p -> UserPassword <$> u <*> pure p) <$> user <*> passwordOnUserChange

            pure (userPassword, errMsg, registerClick)
      pure (userPassword', registerClick')

registerWidget :: forall t m. MonadWidget t m
               => UserClient t m
               -> m (Dynamic t (Maybe UserPassword))
registerWidget UserClient{..} =
    divClass "modal is-active" $ mdo
      divClass "modal-background" $
        notification $ (def :: NotificationConfig t)
          & notificationConfig_show .~ errMsg
      (userPasswordDyn, errMsg) <- divClass "modal-content" $
        divClass "card" $ do
          divClass "card-header" $
            divClass "card-header-title" $ text "校园代办平台 - 注册"
          divClass "card-content" $ mdo
            emailDyn <- divClass "field is-horizontal" $ do
              divClass "field-label is-normal" $
                divClass "label" $ text "邮箱"
              divClass "field-body" $
                divClass "field" $ do
                  v <- divClass "control has-icons-left has-icons-right" $ do
                    ti <- inputElement $ def
                      & initialAttributes .~ ("class" =: "input" <> "type" =: "email" <> "placeholder" =: "校园邮箱")
                      & modifyAttributes .~ modifyEmailInputAttributes
                    elClass "span" "icon is-small is-left" $
                      elClass "i" "fas fa-envelope" blank
                    showWidget emailValid $
                      elClass "span" "icon is-small is-right" $
                        elClass "i" "fas fa-check" blank
                    showWidget (fmap not emailValid) $
                      elClass "span" "icon is-small is-right" $
                        elClass "i" "fas fa-times" blank
                    pure (value ti)
                  showWidget (fmap not emailValid) $
                    elClass "p" "help is-danger" $ text "邮箱格式非法"
                  pure v
            let emailValidEvt = fmap (isJust . parseEmailAddress) (updated emailDyn)
                modifyEmailInputAttributes =
                  fmap (\p -> if p then "class" =: Just "input" else "class" =: Just "input is-danger") emailValidEvt
            emailValid <- holdDyn True emailValidEvt
            (codeDyn, applyCodeErrMsg) <- divClass "field is-horizontal" $ do
              divClass "field-label is-normal" $
                elClass "label" "label" $ text "验证码"
              divClass "field-body" $ do
                codeDyn' <- divClass "field is-expanded" $
                  divClass "control has-icons-left has-icons-right" $ do
                    ti <- inputElement $ def
                      & initialAttributes .~ ("class" =: "input" <> "type" =: "text" <> "placeholder" =: "验证码")
                    elClass "span" "icon is-small is-left" $
                      elClass "i" "fas fa-key" blank
                    pure (value ti)
                errMsg' <- buttonApplyCode _applyCode emailDyn
                pure (codeDyn', errMsg')
            passwordDyn <- divClass "field is-horizontal" $ do
              divClass "field-label is-normal" $
                elClass "label" "lebel" $ text "密码"
              divClass "field-body" $
                divClass "field" $
                  divClass "control has-icons-left" $ do
                    ti <- inputElement $ def
                      & initialAttributes .~ ("class" =: "input" <> "type" =: "password" <> "placeholder" =: "密码")
                      & modifyAttributes .~ modifyPasswordInputAttributes
                    elClass "span" "icon is-small is-left" $
                      elClass "i" "fas fa-key" blank
                    pure (value ti)
            confirmPasswordDyn <- divClass "field is-horizontal" $ do
              divClass "field-label is-normal" $
                elClass "label" "lebel" $ text "确认密码"
              divClass "field-body" $
                divClass "field" $ do
                  v <- divClass "control has-icons-left" $ do
                    ti <- inputElement $ def
                      & initialAttributes .~ ("class" =: "input" <> "type" =: "password" <> "placeholder" =: "确认密码")
                      & modifyAttributes .~ modifyPasswordInputAttributes
                    elClass "span" "icon is-small is-left" $
                      elClass "i" "fas fa-key" blank
                    pure (value ti)
                  showWidget (fmap not passwordValid) $
                    elClass "p" "help is-danger" $ text "两次输入密码不一致"
                  pure v

            let passwordValid = (==) <$> passwordDyn <*> confirmPasswordDyn
                modifyPasswordInputAttributes = fmap
                  (\valid -> if valid then "class" =: Just "input" else "class" =: Just "input is-danger")
                  (updated passwordValid)

            loginEvt <- divClass "field is-grouped" $
              divClass "container" $
                divClass "columns" $
                  divClass "column is-8 is-offset-2" $
                    divClass "control is-expanded" $ do
                      (loginEvt', _) <- buttonAttr ("class" =: "button is-link is-fullwidth is-rounded") $
                        text "登录"
                      pure loginEvt'

            let registerForm = (\email -> RegisterForm (User email "新用户" RoleUser)) <$> emailDyn <*> passwordDyn <*> codeDyn
            registerResult <- _register (fmap Right registerForm) loginEvt
            let registerErrMsg = messageOnReqError "注册失败" registerResult
                basicAuthData = (\email password -> BasicAuthData (T.encodeUtf8 email) (T.encodeUtf8 password)) <$> emailDyn <*> passwordDyn
            registeredUserReqResult <- _getProfile
              (fmap Just basicAuthData)
              (void . filterRight . fmap reqResultToEither $ registerResult)
            let userEitherEvt = fmap reqResultToEither registeredUserReqResult
                loginErrMsg = filterLeft userEitherEvt
                userEvt = filterRight userEitherEvt
                userPasswordEvt = attachPromptlyDynWith (flip UserPassword) passwordDyn userEvt
            userPasswordDyn <- holdDyn Nothing (fmap Just userPasswordEvt)
            pure (userPasswordDyn, leftmost [applyCodeErrMsg, registerErrMsg, loginErrMsg])
      pure userPasswordDyn

buttonApplyCode :: MonadWidget t m
                => ApplyCode t m
                -> Dynamic t Text -- ^email
                -> m (Event t Text) -- ^error message
buttonApplyCode applyCode' emailDyn = mdo
    reqResultEvt <- applyCode' (fmap QParamSome emailDyn) clickEvt
    let errMsgEvt = fmap (const "系统繁忙，请稍后重试") . filterLeft . fmap reqResultToEither $ reqResultEvt
    enableReapplyEvt <- delay 30 clickEvt
    btnEnabled <- holdDyn True $ leftmost [fmap (const False) clickEvt, fmap (const True) enableReapplyEvt]
    let btnAttrDyn = fmap (\p -> if p then mempty else "disabled" =: "") btnEnabled
        btnText = fmap (\p -> if p then "发送验证码" else "30秒后重发") btnEnabled

    clickEvt <- divClass "field" $
      divClass "control" $ do
        (e, _) <- buttonDynAttr (("class" =: "button is-outlined" <>) <$> btnAttrDyn) $
          dynText btnText
        pure e
    pure errMsgEvt

rightMay :: Either e a -> Maybe a
rightMay (Left _)  = Nothing
rightMay (Right x) = Just x
