{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module CROE.Common.API
  ( API
  , Private.User(..)
  ) where

import           Data.Aeson
import           Data.Text               (Text)
import           GHC.Generics            (Generic)
import           Servant.API

import qualified CROE.Common.API.Private as Private
import qualified CROE.Common.API.Public  as Public
import           CROE.Common.Util        (aesonOptions)

type API = "api" :> (Private.API :<|> Public.API)
