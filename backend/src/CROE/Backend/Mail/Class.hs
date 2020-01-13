{-# LANGUAGE FlexibleInstances #-}

module CROE.Backend.Mail.Class
  ( MonadMail(..)
  ) where

import           Control.Exception           (bracket)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Text.Lazy as LazyText
import           Network.HaskellNet.SMTP.SSL hiding (sendPlainTextMail)
import qualified Network.HaskellNet.SMTP.SSL as SMTP

import           CROE.Backend.Mail.Internal

class Monad m => MonadMail m where
  sendPlainTextMail :: Text -- ^receiver
                    -> Text -- ^sender
                    -> Text -- ^subject
                    -> Text -- ^body
                    -> m Bool -- ^whether the email is sent successfully

instance (MonadIO m, HasEnv r) => MonadMail (ReaderT r m) where
  sendPlainTextMail receiver sender subject body = do
    Env config <- asks getEnv
    liftIO $ bracket
      (connectSMTPSSL (_config_smtpHost config))
      closeSMTP $ \conn -> do
        success <- authenticate LOGIN
                                (_config_username config)
                                (_config_password config)
                                conn
        if success
        then do
          SMTP.sendPlainTextMail
            (T.unpack receiver)
            (T.unpack sender)
            (T.unpack subject)
            (LazyText.fromStrict body)
            conn
          pure True
        else pure False

instance MonadMail m => MonadMail (ExceptT e m) where
  sendPlainTextMail x1 x2 x3 x4 = lift $ sendPlainTextMail x1 x2 x3 x4
