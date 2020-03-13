{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module CROE.Common.WebSocket
  ( WsMessage(..)
  , parseWsMessage
  , renderWsMessage
  , _WsMessageChat
  , ChatMessage(..)
  , chatMessage_from
  , chatMessage_to
  , chatMessage_body
  ) where

import           Control.Lens
import           CROE.Common.Util     (aesonOptions)
import           Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import           Data.Int
import           Data.Text
import           GHC.Generics

newtype WsMessage = WsMessageChat ChatMessage
  deriving (Show, Eq, Generic)

parseWsMessage :: LBS.ByteString -> Maybe WsMessage
parseWsMessage = decode

renderWsMessage :: WsMessage -> LBS.ByteString
renderWsMessage = encode

data ChatMessage = ChatMessage
  { _chatMessage_from :: Int64 -- ^from user id
  , _chatMessage_to   :: Int64 -- ^to user id
  , _chatMessage_body :: Text -- ^message body
  } deriving (Show, Eq, Generic)

instance FromJSON WsMessage

instance ToJSON WsMessage

instance FromJSON ChatMessage where
  parseJSON = genericParseJSON aesonOptions

instance ToJSON ChatMessage where
  toJSON = genericToJSON aesonOptions
  toEncoding = genericToEncoding aesonOptions

makePrisms ''WsMessage
makeLenses ''ChatMessage
