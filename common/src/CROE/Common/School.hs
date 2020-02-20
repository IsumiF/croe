{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE StrictData      #-}
{-# LANGUAGE TemplateHaskell #-}

module CROE.Common.School
  ( School(..)
  , school_name
  , school_campusList
  , school_domains
  , SchoolCampus(..)
  , schoolCampus_id
  , schoolCampus_name
  ) where

import           Control.Lens
import           Data.Aeson
import           Data.Int
import           Data.Text        (Text)
import           GHC.Generics

import           CROE.Common.Util (aesonOptions)

data School = School
  { _school_name       :: Text
  , _school_campusList :: [SchoolCampus]
  , _school_domains    :: [Text]
  } deriving (Show, Eq, Generic)

data SchoolCampus = SchoolCampus
  { _schoolCampus_id   :: Int64
  , _schoolCampus_name :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON School where
  parseJSON = genericParseJSON aesonOptions

instance ToJSON School where
  toJSON = genericToJSON aesonOptions
  toEncoding = genericToEncoding aesonOptions

instance FromJSON SchoolCampus where
  parseJSON = genericParseJSON aesonOptions

instance ToJSON SchoolCampus where
  toJSON = genericToJSON aesonOptions
  toEncoding = genericToEncoding aesonOptions

makeLenses ''School
makeLenses ''SchoolCampus
