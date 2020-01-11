{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module CROE.Frontend.User.Base
  ( newEnv
  , Env(..)
  , HasEnv(..)
  , UserPassword(..)
  ) where

import Control.Monad.IO.Class
import           Data.IORef
import           Data.Text       (Text)
import           Reflex.Dom

import qualified CROE.Common.API as Common (User (..))

newtype Env t = Env
  { _env_user :: IORef (Dynamic t (Maybe UserPassword))
  }

class Reflex t => HasEnv r t where
  getEnv :: r -> Env t

instance Reflex t => HasEnv (Env t) t where
  getEnv = id

newEnv :: (Reflex t, MonadIO m) => m (Env t)
newEnv = do
    initial <- liftIO $ newIORef (constDyn Nothing)
    pure $ Env initial

data UserPassword = UserPassword
  { _user_profile  :: Common.User
  , _user_password :: Text
  }
