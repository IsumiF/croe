{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module CROE.Backend.Persist.Types
  ( User(..)
  , Role(..)
  , UserId
  , EntityField(..)
  , migrateAll
  , Unique(UniqueUserEmail)
  ) where

import           Data.ByteString                 (ByteString)
import           Data.Text                       (Text)
import           Database.Persist
import           Database.Persist.TH

import           CROE.Backend.Persist.Types.Role

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
  email Text
  name Text
  hashedPassword ByteString
  role Role
  UniqueUserEmail email
  deriving Show
|]