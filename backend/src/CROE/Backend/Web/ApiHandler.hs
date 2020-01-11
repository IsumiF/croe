{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module CROE.Backend.Web.ApiHandler
  ( apiHandler
  ) where

import           Control.Monad.Except
import           Servant

import           CROE.Backend.Env
import           CROE.Backend.Persist.Base (backend)
import qualified CROE.Backend.Service.User as User
import           CROE.Common.API

apiHandler :: ServerT API (ExceptT ServerError App)
apiHandler = (\_ _ -> pure "hello") :<|> User.register backend
