{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module CROE.Frontend.Client.Class.User
  ( MonadUser(..)
  ) where

import           Control.Monad.Reader
import           Data.Proxy                    (Proxy (..))
import           Data.Text                     (Text)
import           Reflex.Dom
import           Servant.API
import           Servant.Reflex

import           CROE.Common.API.User
import           CROE.Frontend.Client.Internal

class (Reflex t, Monad m) => MonadUser t m where
  putProfile :: PutProfile t m
  register :: Register t m

instance (MonadWidget t m, HasEnv r) => MonadUser t (ReaderT r m) where
  putProfile x1 x2 x3 = do
    Funcs{..} <- client'
    _putProfile x1 x2 x3
  register x1 x2 = do
    Funcs{..} <- client'
    _register x1 x2

client' :: forall t m r. (MonadWidget t m, MonadReader r m, HasEnv r) 
        => m (Funcs t m)
client' = do
    Env baseUrl <- asks getEnv
    let (_putProfile :<|> _register) = client (Proxy :: Proxy API) (Proxy :: Proxy m) (Proxy :: Proxy ()) (constDyn baseUrl)
    pure Funcs{..}

data Funcs t m = Funcs
  { _putProfile :: PutProfile t m
  , _register   :: Register t m
  }

type PutProfile t m = Dynamic t (Maybe BasicAuthData)
                   -> Dynamic t (Either Text User)
                   -> Event t ()
                   -> m (Event t (ReqResult () Text))

type Register t m = Dynamic t (QParam Text)
                 -> Event t ()
                 -> m (Event t (ReqResult () NoContent))

{-
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

-}
