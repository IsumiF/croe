{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module CROE.Common.API.WithTotal
  ( WithTotal(..)
  , withTotal_data
  , withTotal_total
  ) where

import           Control.Lens
import           CROE.Common.Util (aesonOptions)
import           Data.Aeson
import           GHC.Generics

data WithTotal a = WithTotal
  { _withTotal_data  :: a
  , _withTotal_total :: Int
  } deriving (Show, Eq, Generic)

instance FromJSON a => FromJSON (WithTotal a) where
  parseJSON = genericParseJSON aesonOptions

instance ToJSON a => ToJSON (WithTotal a) where
  toJSON = genericToJSON aesonOptions
  toEncoding = genericToEncoding aesonOptions

makeLenses ''WithTotal
