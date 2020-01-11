{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module CROE.Frontend.Env
  ( AppT
  , newEnv
  , Env
  ) where

import           Control.Monad.Reader
import           Data.Aeson
import qualified Data.ByteString.Lazy      as LBS
import           Data.Proxy                (Proxy (..))
import           GHC.Generics              (Generic)
import           Reflex.Dom

import           CROE.Common.Util          (aesonOptions)
import qualified CROE.Frontend.Client.Base as Client
import           CROE.Frontend.Env.TH
import qualified CROE.Frontend.User.Base   as User

type AppT t m = ReaderT (Env t m) m

data Env t m = Env
  { _env_client :: Client.Env t m
  , _env_user   :: User.Env t
  } deriving Generic

newEnv :: forall t m. MonadWidget t m => m (Env t m)
newEnv = do
    _env_user <- User.newEnv
    let _env_client = Client.newEnv (Proxy :: Proxy m) (_config_client config')
    pure Env{..}
  where
    config' =
      case config of
        Left msg -> error msg
        Right v -> v

newtype Config = Config
  { _config_client :: Client.Config
  } deriving Generic

instance FromJSON Config where
  parseJSON = genericParseJSON aesonOptions

config :: Either String Config
config = eitherDecode' (LBS.fromStrict bs)
  where
    bs = $(embedConfig "config")