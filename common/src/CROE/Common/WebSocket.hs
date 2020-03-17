{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module CROE.Common.WebSocket
  ( WsMessage(..)
  , parseWsMessage
  , renderWsMessage
  , _WsSendChatMessage
  , _WsReceiveChatMessage
  , module Export
  ) where

import           Control.Lens
import           CROE.Common.Chat
import qualified CROE.Common.Chat     as Export
import           CROE.Common.Util     (aesonOptions)
import           Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import           Data.Int
import           Data.Text
import           Data.Time
import           GHC.Generics

data WsMessage = WsSendChatMessage SendChatMessage
               | WsReceiveChatMessage ReceiveChatMessage
  deriving (Show, Eq, Generic)

instance FromJSON WsMessage

instance ToJSON WsMessage

parseWsMessage :: LBS.ByteString -> Maybe WsMessage
parseWsMessage = decode

renderWsMessage :: WsMessage -> LBS.ByteString
renderWsMessage = encode

makePrisms ''WsMessage
