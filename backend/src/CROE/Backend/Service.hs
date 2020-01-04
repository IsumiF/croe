{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module CROE.Backend.Service
  ( login
  ) where

import           Control.Monad.Except
import           Data.Text            (Text)
import           Servant

import           CROE.Common.API

login :: (MonadError ServantErr m)
      => UserCredential
      -> m Text
login _ = pure "jwt token"
