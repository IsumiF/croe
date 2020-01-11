{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module CROE.Common.API.Public
  ( API
  , APIRegister
  ) where

import           Data.Text   (Text)
import           Servant.API

type API = "public" :> APIRegister

type APIRegister = "users" :> QueryParam "email" Text :> Post '[JSON] NoContent
