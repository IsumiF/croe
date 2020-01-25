{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators     #-}

module CROE.Backend.Web.ApiHandler
  ( apiHandler
  ) where

import           Control.Monad.Except
import           Servant

import           CROE.Backend.Env
import           CROE.Backend.Persist.Base (proxy)
import qualified CROE.Backend.Service.User as User
import           CROE.Common.API

apiHandler :: ServerT API (ExceptT ServerError App)
apiHandler =
    (\user -> User.putProfile proxy user
    :<|> User.getProfile proxy user
    )
    :<|> User.applyCode proxy
    :<|> User.register proxy
    :<|> User.validateEmail proxy
