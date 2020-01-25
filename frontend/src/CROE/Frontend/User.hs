{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module CROE.Frontend.User
  ( UserPassword(..)
  , userPassword_user
  , userPassword_password
  , toBasicAuthData
  , User(..)
  , user_email
  , user_name
  ) where

import           Control.Lens
import           Data.Text            (Text)
import           Data.Text.Encoding   as T
import           GHC.Generics
import           Servant.API

import           CROE.Common.API.User

data UserPassword = UserPassword
  { _userPassword_user     :: User
  , _userPassword_password :: Text
  } deriving (Show, Eq, Generic)

toBasicAuthData :: UserPassword -> BasicAuthData
toBasicAuthData (UserPassword user pwd) =
    BasicAuthData (T.encodeUtf8 email) (T.encodeUtf8 pwd)
  where
    email = _user_email user

makeLenses ''UserPassword
