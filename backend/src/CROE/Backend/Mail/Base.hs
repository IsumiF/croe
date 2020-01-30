module CROE.Backend.Mail.Base
  ( Config
  , Env
  , newEnv
  , runClient
  ) where

import           Control.Exception           (bracket)
import           Control.Monad.Except
import           Data.Aeson
import qualified Data.Text                   as T
import qualified Data.Text.Lazy              as LazyText
import           GHC.Generics                (Generic)
import           Network.HaskellNet.SMTP.SSL hiding (sendPlainTextMail)
import qualified Network.HaskellNet.SMTP.SSL as SMTP
import           Polysemy

import           CROE.Backend.Mail.Class
import           CROE.Common.Util            (aesonOptions)

data Config = Config
  { _config_smtpHost :: String
  , _config_username :: String
  , _config_password :: String
  } deriving (Show, Eq, Generic)

instance FromJSON Config where
  parseJSON = genericParseJSON aesonOptions

newtype Env = Env Config

newEnv :: Config -> Env
newEnv = Env

runClient :: Member (Embed IO) r
          => Env
          -> Sem (Client : r) a
          -> Sem r a
runClient (Env config) = interpret $ \case
    SendPlainTextMail receiver sender subject body ->
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