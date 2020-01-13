{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module CROE.Common.API.User
  ( API
  , User(..)
  ) where

import           Data.Aeson
import           Data.Text        (Text)
import           GHC.Generics     (Generic)
import           Servant.API

import           CROE.Common.Util (aesonOptions)

type API = "user" :>
  ( BasicAuth "croe" User :> APIPutProfile
  :<|> APIRegister
  )

type APIPutProfile = "profile"
  :> ReqBody '[JSON] User :> Put '[JSON] Text

type APIRegister = "register" :> QueryParam "email" Text :> Post '[JSON] NoContent

data User = User
  { _user_email :: Text
  , _user_name  :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON User where
  parseJSON = genericParseJSON aesonOptions

instance ToJSON User where
  toJSON = genericToJSON aesonOptions
  toEncoding = genericToEncoding aesonOptions
