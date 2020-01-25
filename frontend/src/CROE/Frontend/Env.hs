{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module CROE.Frontend.Env
  ( AppT
  , newEnv
  , Env(..)
  , env_client
  ) where

import           Control.Lens
import           Control.Monad.Reader
import           Data.Aeson
import qualified Data.ByteString.Lazy      as LBS
import           GHC.Generics              (Generic)
import           Reflex.Dom                hiding (Client)

import           CROE.Common.Util          (aesonOptions)
import           CROE.Frontend.Client.Init (Client)
import qualified CROE.Frontend.Client.Init as Client
import           CROE.Frontend.Env.TH

type AppT t m = ReaderT (Env t m) m

newtype Env t m = Env
  { _env_client :: Client t m
  } deriving Generic

makeLenses ''Env

newEnv :: forall t m. MonadWidget t m => m (Env t m)
newEnv = do
    let _env_client = Client.newClient (_config_client config')
    pure Env{..}
  where
    config' =
      case config of
        Left msg -> error msg
        Right v  -> v

newtype Config = Config
  { _config_client :: Client.Config
  } deriving (Generic, Show, Eq)

instance FromJSON Config where
  parseJSON = genericParseJSON aesonOptions

config :: Either String Config
config = eitherDecode' (LBS.fromStrict bs)
  where
    bs = $(embedConfig "config")
