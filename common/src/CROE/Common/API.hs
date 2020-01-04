{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module CROE.Common.API
  ( API
  , APILogin
  , UserCredential(..)
  ) where

import           Data.Aeson
import           Data.Text        (Text)
import           GHC.Generics     (Generic)
import           Servant.API

import           CROE.Common.Util (aesonOptions)

type API = "api" :> "user" :> APILogin

type APILogin = "login" :> ReqBody '[JSON] UserCredential :> Post '[JSON] Text

data UserCredential = UserCredential
  { _userCredential_email    :: Text
  , _userCredential_password :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON UserCredential where
  toJSON = genericToJSON aesonOptions
  toEncoding = genericToEncoding aesonOptions

instance FromJSON UserCredential where
  parseJSON = genericParseJSON aesonOptions
