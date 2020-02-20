{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module CROE.Common.API.School
  ( API
  , module CROE.Common.School
  ) where

import           Servant.API

import           CROE.Common.School

type API = "school" :> APIGet

type APIGet = Get '[JSON] [School]
