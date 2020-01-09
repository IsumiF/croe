{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module CROE.Backend.Service
  ( 
  ) where

import           Control.Monad.Except
import           Data.Text            (Text)
import           Servant

import           CROE.Common.API

-- login :: (MonadError ServerError m)
--       => 
--       -> m Text
-- login _ = pure "jwt token"
