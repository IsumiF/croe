{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}

module CROE.Frontend.Client.Base
  ( Env
  , newEnv
  , Config
  ) where

import           Data.Aeson
import           Data.Proxy       (Proxy (..))
import           Data.Text        (Text)
import           GHC.Generics     (Generic)
import           Reflex.Dom
import           Servant.API
import           Servant.Reflex

import           CROE.Common.API
import           CROE.Common.Util (aesonOptions)

newtype Config = Config
    { _config_baseUrl :: BaseUrl
    } deriving Generic

instance FromJSON Config where
  parseJSON = genericParseJSON aesonOptions

newtype Env t m = Env (ClientFuncs t m)

data ClientFuncs t m = ClientFuncs
  { _putUserProfile :: Dynamic t (Maybe BasicAuthData)
                    -> Dynamic t (Either Text User)
                    -> Event t ()
                    -> m (Event t (ReqResult () Text))
  , _register :: Dynamic t (QParam Text)
              -> Event t () -> m (Event t (ReqResult () NoContent))
  } deriving Generic

newEnv :: MonadWidget t m => Proxy m -> Config -> Env t m
newEnv p (Config baseUrl) =
    let (_putUserProfile :<|> _register) =
          client (Proxy :: Proxy API) p (Proxy :: Proxy ())
            (constDyn baseUrl)
     in Env (ClientFuncs{..})
