{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE TemplateHaskell   #-}

module CROE.Common.User
  ( User(..)
  , user_email
  , user_name
  , user_role
  , user_id
  , Role(..)
  , _RoleAdmin
  , _RoleUser
  , EmailAddress(..)
  , emailAddress_local
  , emailAddress_domain
  , parseEmailAddress
  , showEmailAddress
  ) where

import           Control.Lens
import           Data.Aeson
import           Data.ByteString     (ByteString)
import           Data.Int
import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as Map
import           Data.Text           (Text)
import qualified Data.Text.Encoding  as T
import           GHC.Generics        (Generic)
import qualified Text.Email.Validate as Validate

import           CROE.Common.Util    (aesonOptions, reverseMap)

data User = User
  { _user_email :: Text
  , _user_name  :: Text
  , _user_role  :: Role
  , _user_id    :: Int64
  } deriving (Show, Eq, Generic)

data Role = RoleAdmin | RoleUser
            deriving (Show, Read, Eq, Ord, Generic)

data EmailAddress = EmailAddress
  { _emailAddress_local  :: Text
  , _emailAddress_domain :: Text
  } deriving (Show, Eq, Generic)

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

instance FromJSON User where
  parseJSON = genericParseJSON aesonOptions

instance ToJSON User where
  toJSON = genericToJSON aesonOptions
  toEncoding = genericToEncoding aesonOptions

instance FromJSON Role where
  parseJSON = genericParseJSON aesonOptions

instance ToJSON Role where
  toJSON = genericToJSON aesonOptions
  toEncoding = genericToEncoding aesonOptions

instance FromJSON EmailAddress where
  parseJSON v = do
    email <- genericParseJSON aesonOptions v
    case parseEmailAddress (showEmailAddress email) of
      Nothing     -> fail "illegal email format"
      Just email' -> pure email'

instance ToJSON EmailAddress where
  toEncoding = genericToEncoding aesonOptions
  toJSON = genericToJSON aesonOptions

makeLenses ''User
makePrisms ''Role
makeLenses ''EmailAddress
