{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module CROE.Backend.Persist.Types.Role
  ( Role(..)
  ) where

import           Data.Bifunctor
import           Database.Persist.TH

import qualified CROE.Common.User    as Common (Role (..))

newtype Role = Role Common.Role

instance Show Role where
  show (Role r) = show r

instance Read Role where
  readsPrec a b = first Role <$> readsPrec a b

derivePersistField "Role"
