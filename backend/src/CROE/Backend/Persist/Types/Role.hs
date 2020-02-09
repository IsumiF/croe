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

import           Database.Persist.TH

import qualified CROE.Common.User    as Common (Role (..))

newtype Role = Role Common.Role
  deriving (Show, Read)

derivePersistField "Role"
