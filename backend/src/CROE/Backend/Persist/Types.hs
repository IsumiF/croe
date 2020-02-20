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
  , userToCommon
  , UserRegistry(..)
  , UserRegistryId
  , School(..)
  , SchoolId
  , SchoolDomain(..)
  , SchoolDomainId
  , SchoolCampus(..)
  , SchoolCampusId
  , Task(..)
  , TaskId
  , TaskStatus(..)
  , Duration
  , Review(..)
  , ReviewId
  , EntityField(..)
  , migrateAll
  , Unique(UniqueUserEmail)
  ) where

import           Data.ByteString                       (ByteString)
import           Data.Coerce
import           Data.Text                             (Text)
import           Data.Time
import           Data.Word                             (Word64)
import           Database.Persist
import           Database.Persist.Sql                  (fromSqlKey)
import           Database.Persist.TH

import           CROE.Backend.Persist.Types.Role
import           CROE.Backend.Persist.Types.TaskStatus
import qualified CROE.Common.User                      as Common

type Duration = (UTCTime, UTCTime)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
  email Text
  name Text
  hashedPassword ByteString
  role Role
  balance Word64
  UniqueUserEmail email
  deriving Show
UserRegistry
  email Text
  createdAt UTCTime
  code Text
  deriving Show
School
  name Text
  deriving Show
SchoolDomain
  schoolId SchoolId
  domain Text
  deriving Show
SchoolCampus
  schoolId SchoolId
  name Text
  deriving Show
Task
  currentStatus TaskStatus
  creator UserId
  taker UserId Maybe
  reward Word64
  title Text
  location SchoolCampusId
  duration Duration
  abstract Text
  descriptionKey Text
  deriving Show
Review
  user UserId
  from UserId Maybe
  rating Double
  message Text Maybe
  deriving Show
|]

userToCommon :: Entity User -> Common.User
userToCommon (Entity userId user) = Common.User
    (userEmail user)
    (userName user)
    (coerce (userRole user))
    (fromSqlKey userId)
