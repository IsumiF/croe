{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

module CROE.Common.API.User
  ( API
  , RegisterForm(..)
  , registerForm_user
  , registerForm_password
  , registerForm_code
  , module CROE.Common.User
  ) where

import           Control.Lens
import           Data.Aeson
import           Data.Text        (Text)
import           GHC.Generics     (Generic)
import           Servant.API

import           CROE.Common.User
import           CROE.Common.Util (aesonOptions)

type API = "user" :>
  ( BasicAuth "croe" User :> (APIPutProfile :<|> APIGetProfile)
  :<|> APIApplyCode
  :<|> APIRegister
  :<|> APIValidateEmail
  )

type APIGetProfile = "profile"
  :> Get '[JSON] User

type APIPutProfile = "profile"
  :> ReqBody '[JSON] User :> Put '[JSON] NoContent

type APIApplyCode = "apply_code"
  :> QueryParam "email" Text
  :> Post '[JSON] NoContent

type APIRegister = "register"
  :> ReqBody '[JSON] RegisterForm
  :> Post '[JSON] NoContent

-- 校验邮箱，返回学校名
type APIValidateEmail = "validate_email"
  :> ReqBody '[JSON] EmailAddress
  :> Post '[JSON] Text

data RegisterForm = RegisterForm
  { _registerForm_user     :: User
  , _registerForm_password :: Text
  , _registerForm_code     :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON RegisterForm where
  parseJSON = genericParseJSON aesonOptions

instance ToJSON RegisterForm where
  toJSON = genericToJSON aesonOptions
  toEncoding = genericToEncoding aesonOptions

makeLenses ''RegisterForm
