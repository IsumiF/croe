{-# LANGUAGE DeriveGeneric #-}

module CROE.Frontend.Client.Internal where

import           Data.Aeson
import           GHC.Generics     (Generic)
import           Servant.Reflex

import           CROE.Common.Util (aesonOptions)

newtype Config = Config
    { _config_baseUrl :: BaseUrl
    } deriving Generic

instance FromJSON Config where
  parseJSON = genericParseJSON aesonOptions

newtype Env = Env BaseUrl

newEnv :: Config -> Env
newEnv (Config baseUrl) = Env baseUrl

class HasEnv r where
  getEnv :: r -> Env

instance HasEnv Env where
  getEnv = id
