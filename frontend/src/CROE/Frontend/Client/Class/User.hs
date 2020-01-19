{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module CROE.Frontend.Client.Class.User
  ( ClientFuncs(..)
  , initFuncs
  ) where

import           Control.Monad.Reader
import           Data.Proxy                    (Proxy (..))
import           Data.Text                     (Text)
import           Reflex.Dom
import           Servant.API
import           Servant.Reflex

import           CROE.Common.API.User
import           CROE.Frontend.Client.Internal

initFuncs :: forall t m r. (MonadWidget t m, MonadReader r m, HasEnv r)
          => m (ClientFuncs t m)
initFuncs = do
    Env baseUrl <- asks getEnv
    let (_putProfile :<|> _applyCode :<|> _register) = client (Proxy :: Proxy API) (Proxy :: Proxy m) (Proxy :: Proxy ()) (constDyn baseUrl)
    pure ClientFuncs{..}

data ClientFuncs t m = ClientFuncs
  { _putProfile :: PutProfile t m
  , _applyCode  :: ApplyCode t m
  , _register   :: Register t m
  }

type PutProfile t m = Dynamic t (Maybe BasicAuthData)
                   -> Dynamic t (Either Text User)
                   -> Event t ()
                   -> m (Event t (ReqResult () NoContent))

type ApplyCode t m = Dynamic t (QParam Text)
                  -> Event t ()
                  -> m (Event t (ReqResult () NoContent))

type Register t m = Dynamic t (Either Text RegisterForm)
                 -> Event t ()
                 -> m (Event t (ReqResult () NoContent))
