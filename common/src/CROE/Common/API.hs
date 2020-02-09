{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module CROE.Common.API
  ( API
  , User.User(..)
  ) where

import           Servant.API

import qualified CROE.Common.API.Task as Task
import qualified CROE.Common.API.User as User
import           CROE.Common.User     (User)

type API = "api" :>
  ( User.API
  :<|> BasicAuth "croe" User :> Task.API
  )
