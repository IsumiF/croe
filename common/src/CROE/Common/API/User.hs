{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module CROE.Common.API.User
  ( API
  , User(..)
  , RegisterForm(..)
  ) where

import           Data.Aeson
import           Data.Text        (Text)
import           GHC.Generics     (Generic)
import           Servant.API

import           CROE.Common.Util (aesonOptions)

type API = "user" :>
  ( BasicAuth "croe" User :> APIPutProfile
  :<|> APIApplyCode
  :<|> APIRegister
  )

type APIPutProfile = "profile"
  :> ReqBody '[JSON] User :> Put '[JSON] NoContent

type APIApplyCode = "apply_code"
  :> QueryParam "email" Text
  :> Post '[JSON] NoContent

type APIRegister = "register"
  :> ReqBody '[JSON] RegisterForm
  :> Post '[JSON] NoContent

data User = User
  { _user_email :: Text
  , _user_name  :: Text
  } deriving (Show, Eq, Generic)

data RegisterForm = RegisterForm
  { _registerForm_user     :: User
  , _registerForm_password :: Text
  , _registerForm_code     :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON User where
  parseJSON = genericParseJSON aesonOptions

instance ToJSON User where
  toJSON = genericToJSON aesonOptions
  toEncoding = genericToEncoding aesonOptions

instance FromJSON RegisterForm where
  parseJSON = genericParseJSON aesonOptions

instance ToJSON RegisterForm where
  toJSON = genericToJSON aesonOptions
  toEncoding = genericToEncoding aesonOptions
