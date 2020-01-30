module CROE.Backend.Mail.Class
  ( Client(..)
  , sendPlainTextMail
  ) where

import           Data.Text                   (Text)
import           Polysemy

data Client (m :: * -> *) a where
  SendPlainTextMail :: Text -- ^receiver
                    -> Text -- ^sender
                    -> Text -- ^subject
                    -> Text -- ^body
                    -> Client m Bool -- ^whether the email is sent successfully

makeSem ''Client
