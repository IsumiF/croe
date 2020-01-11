{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module CROE.Frontend.User.Class
  ( MonadUser(..)
  , getBasicAuthData
  ) where

import           Control.Monad.Reader
import           Data.IORef
import qualified Data.Text.Encoding      as T
import           Reflex.Dom
import           Servant.API

import qualified CROE.Common.API         as Common (User (..))
import           CROE.Frontend.User.Base

class (Reflex t, Monad m) => MonadUser t m where
  -- |Initialze user. All following calls to 'getUserDyn', before another call
  -- to 'initUserDyn', will use the user provided here.
  initUserDyn :: Dynamic t (Maybe UserPassword) -> m ()
  getUserDyn :: m (Dynamic t (Maybe UserPassword))

instance (MonadWidget t m, HasEnv r t) => MonadUser t (ReaderT r m) where
  initUserDyn newUser = do
    Env userIORef <- asks getEnv
    liftIO $ atomicWriteIORef userIORef newUser
  getUserDyn = do
    Env userIORef <- asks getEnv
    liftIO $ readIORef userIORef

getBasicAuthData :: (Reflex t, MonadUser t m)
                 => m (Dynamic t (Maybe BasicAuthData))
getBasicAuthData =
    (fmap . fmap) toBasicAuthData <$> getUserDyn

toBasicAuthData :: UserPassword -> BasicAuthData
toBasicAuthData x = BasicAuthData email pwd
  where
    email = T.encodeUtf8 . Common._user_email . _user_profile $ x
    pwd = T.encodeUtf8 . _user_password $ x
