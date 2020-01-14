module CROE.Backend.Time.Class
  ( MonadTime(..)
  ) where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Time            hiding (getCurrentTime)
import qualified Data.Time            as Time (getCurrentTime)

class Monad m => MonadTime m where
  getCurrentTime :: m UTCTime

instance MonadIO m => MonadTime (ReaderT r m) where
  getCurrentTime = liftIO Time.getCurrentTime

instance MonadTime m => MonadTime (ExceptT e m) where
  getCurrentTime = lift getCurrentTime
