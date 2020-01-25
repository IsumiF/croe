{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module CROE.Common.User
  ( EmailAddress(..)
  , emailAddress_local
  , emailAddress_domain
  , parseEmailAddress
  , showEmailAddress
  ) where

import           Control.Lens
import           Data.Aeson
import           Data.ByteString     (ByteString)
import           Data.Text           (Text)
import qualified Data.Text.Encoding  as T
import           GHC.Generics        (Generic)
import qualified Text.Email.Validate as Validate

import           CROE.Common.Util    (aesonOptions)

data EmailAddress = EmailAddress
  { _emailAddress_local  :: Text
  , _emailAddress_domain :: Text
  } deriving (Show, Eq, Generic)

makeLenses ''EmailAddress

parseEmailAddress :: Text -> Maybe EmailAddress
parseEmailAddress =
    fmap convert . Validate.emailAddress . T.encodeUtf8
  where
    convert email =
      EmailAddress
        (T.decodeUtf8 (Validate.localPart email))
        (T.decodeUtf8 (Validate.domainPart email))

showEmailAddress :: EmailAddress -> Text
showEmailAddress (EmailAddress local domain) = local <> "@" <> domain

instance FromJSON EmailAddress where
  parseJSON v = do
    email <- genericParseJSON aesonOptions v
    case parseEmailAddress (showEmailAddress email) of
      Nothing -> fail "illegal email format"
      Just email' -> pure email'

instance ToJSON EmailAddress where
  toEncoding = genericToEncoding aesonOptions
  toJSON = genericToJSON aesonOptions
