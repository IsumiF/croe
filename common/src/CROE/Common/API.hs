{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module CROE.Common.API
  ( API
  , User.User(..)
  ) where

import           Servant.API

import qualified CROE.Common.API.User as User

type API = "api" :> User.API
