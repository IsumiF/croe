{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE StrictData      #-}
{-# LANGUAGE TemplateHaskell #-}

module CROE.Common.Chat
  ( SendChatMessage(..)
  , sendChatMessage_to
  , sendChatMessage_body
  , ReceiveChatMessage(..)
  , receiveChatMessage_from
  , receiveChatMessage_body
  , receiveChatMessage_time
  , ChatMessageStatus(..)
  , _CmsRead
  , _CmsUnread
  , Contact(..)
  , contact_user
  , contact_unreadCount
  ) where

import           Control.Lens
import           CROE.Common.User
import           CROE.Common.Util (aesonOptions)
import           Data.Aeson
import           Data.Int
import           Data.Text
import           Data.Time
import           GHC.Generics

data SendChatMessage = SendChatMessage
  { _sendChatMessage_to   :: Int64 -- ^to user id
  , _sendChatMessage_body :: Text -- ^message body
  } deriving (Show, Eq, Generic)

data ReceiveChatMessage = ReceiveChatMessage
  { _receiveChatMessage_id     :: Int64 -- ^message id
  , _receiveChatMessage_from   :: Int64 -- ^from user id
  , _receiveChatMessage_body   :: Text -- ^message body
  , _receiveChatMessage_time   :: UTCTime -- ^time sent
  , _receiveChatMessage_status :: ChatMessageStatus
  } deriving (Show, Eq, Generic)

data ChatMessageStatus = CmsRead
                       | CmsUnread
                         deriving (Show, Read, Eq, Ord, Enum, Generic)

instance FromJSON SendChatMessage where
  parseJSON = genericParseJSON aesonOptions

instance ToJSON SendChatMessage where
  toJSON = genericToJSON aesonOptions
  toEncoding = genericToEncoding aesonOptions

instance FromJSON ReceiveChatMessage where
  parseJSON = genericParseJSON aesonOptions

instance ToJSON ReceiveChatMessage where
  toJSON = genericToJSON aesonOptions
  toEncoding = genericToEncoding aesonOptions

instance FromJSON ChatMessageStatus where
  parseJSON = genericParseJSON aesonOptions

instance ToJSON ChatMessageStatus where
  toJSON = genericToJSON aesonOptions
  toEncoding = genericToEncoding aesonOptions

makeLenses ''SendChatMessage
makeLenses ''ReceiveChatMessage
makePrisms ''ChatMessageStatus

data Contact = Contact
  { _contact_user        :: User
  , _contact_unreadCount :: Int
  } deriving (Show, Eq, Generic)

instance FromJSON Contact where
  parseJSON = genericParseJSON aesonOptions

instance ToJSON Contact where
  toJSON = genericToJSON aesonOptions
  toEncoding = genericToEncoding aesonOptions

makeLenses ''Contact
