{-# LANGUAGE DeriveGeneric #-}

module CROE.Backend.Mail.Internal where

import           Data.Aeson
import           GHC.Generics     (Generic)

import           CROE.Common.Util (aesonOptions)

data Config = Config
  { _config_smtpHost :: String
  , _config_username :: String
  , _config_password :: String
  } deriving (Show, Eq, Generic)

instance FromJSON Config where
  parseJSON = genericParseJSON aesonOptions

newtype Env = Env Config

newEnv :: Config -> Env
newEnv = Env

class HasEnv r where
  getEnv :: r -> Env

instance HasEnv Env where
  getEnv = id
