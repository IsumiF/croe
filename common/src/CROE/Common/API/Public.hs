{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module CROE.Common.API.Public
  ( API
  ) where

import           Data.Text   (Text)
import           Servant.API

type API = "public" :> APIRegister

type APIRegister = "users" :> QueryParam "email" Text :> Post '[JSON] NoContent
