{-# LANGUAGE FlexibleContexts #-}

module CROE.Backend.Service.User
  (
  ) where

import           Control.Monad.Error
import           Data.Text           (Text)
import           Servant

register :: MonadError ServerError m
         => Maybe Text
         -> m ()
register = undefined
