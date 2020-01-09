{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module CROE.Common.API.Private
  ( API
  , User(..)
  ) where

import           Data.Aeson
import           Data.Text        (Text)
import           GHC.Generics     (Generic)
import           Servant.API

import           CROE.Common.Util (aesonOptions)

type API = "private" :> BasicAuth "croe-priavte" User :> APIUserProfile

type APIUserProfile = "users" :> "profile"
  :> ReqBody '[JSON] User :> Put '[JSON] Text

data User = User
  { _user_email :: Text
  , _user_name  :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON User where
  parseJSON = genericParseJSON aesonOptions

instance ToJSON User where
  toJSON = genericToJSON aesonOptions
  toEncoding = genericToEncoding aesonOptions
