{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module CROE.Backend.Service.User
  ( register
  ) where

import           Control.Monad.Except
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.Proxy                 (Proxy)
import           Data.Text                  (Text)
import           Servant

import           CROE.Backend.Persist.Class

register :: ( MonadError ServerError m
            , MonadPersist backend m
            , WriteEntity User (ReaderT backend m)
            , MonadLogger m
            )
         => Proxy backend
         -> Maybe Text
         -> m NoContent
register _ email = do
    case email of
      Just email' -> $(logDebug) email'
      Nothing     -> $(logDebug) "Nothing"
    pure NoContent
