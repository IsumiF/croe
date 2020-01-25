{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module CROE.Frontend.Client.User
  ( UserClient(..)
  , putProfile
  , getProfile
  , applyCode
  , register
  , validateEmail
  , PutProfile
  , GetProfile
  , ApplyCode
  , Register
  , ValidateEmail
  ) where

import           Control.Lens
import           Data.Text            (Text)
import           Reflex.Dom
import           Servant.API
import           Servant.Reflex

import           CROE.Common.API.User
import           CROE.Common.User

data UserClient t m = UserClient
  { _putProfile    :: PutProfile t m
  , _getProfile    :: GetProfile t m
  , _applyCode     :: ApplyCode t m
  , _register      :: Register t m
  , _validateEmail :: ValidateEmail t m
  }

type PutProfile t m = Dynamic t (Maybe BasicAuthData)
                   -> Dynamic t (Either Text User)
                   -> Event t ()
                   -> m (Event t (ReqResult () NoContent))

type GetProfile t m = Dynamic t (Maybe BasicAuthData)
                   -> Event t ()
                   -> m (Event t (ReqResult () User))

type ApplyCode t m = Dynamic t (QParam Text)
                  -> Event t ()
                  -> m (Event t (ReqResult () NoContent))

type Register t m = Dynamic t (Either Text RegisterForm)
                 -> Event t ()
                 -> m (Event t (ReqResult () NoContent))

type ValidateEmail t m = Dynamic t (Either Text EmailAddress)
                       -> Event t ()
                       -> m (Event t (ReqResult () Text))

makeLenses ''UserClient
