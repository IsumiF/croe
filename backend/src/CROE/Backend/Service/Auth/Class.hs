module CROE.Backend.Service.Auth.Class
  ( AuthService(..)
  , checkBasicAuth
  , hashPassword
  , invalidateCache
  ) where

import           Data.ByteString (ByteString)
import           Polysemy
import           Servant

import qualified CROE.Common.API as Common

data AuthService (m :: * -> *) r where
  CheckBasicAuth :: BasicAuthData -> AuthService m (BasicAuthResult Common.User)
  HashPassword :: ByteString -> AuthService m ByteString
  InvalidateCache :: BasicAuthData -> AuthService m ()

makeSem ''AuthService
