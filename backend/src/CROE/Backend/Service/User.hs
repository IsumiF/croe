{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module CROE.Backend.Service.User
  ( register
  -- *Inernal
  ) where

import           Control.Monad.Except
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.Proxy                 (Proxy)
import           Data.Text                  (Text)
import           Servant

import           CROE.Backend.Mail.Class
import           CROE.Backend.Persist.Class

register :: ( MonadError ServerError m
            , MonadPersist backend m
            , WriteEntity User (ReaderT backend m)
            , MonadLogger m
            , MonadMail m
            )
         => Proxy backend
         -> Maybe Text
         -> m NoContent
register _ email =
    case email of
      Just email' -> do
        $(logInfo) $ email' <> " is registering"
        success <- sendPlainTextMail email' "CROE" "校园代办：注册验证码" (registerMailBody "123456")
        if success
        then pure NoContent
        else throwError err500 { errBody = "系统繁忙，请稍后重试" }
      Nothing     ->
        throwError err400 { errBody = "必须提供合法的邮箱地址" }

registerMailBody :: Text
                 -> Text
registerMailBody code = "验证码是：" <> code <> "，30分钟内有效"
