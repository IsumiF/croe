module CROE.Backend.Service.Chat.Class
  ( ChatService(..)
  , sendMessage
  , receiveMessage
  ) where

import           Control.Concurrent.MVar
import           CROE.Common.Chat        (ReceiveChatMessage,
                                          SendChatMessage (..))
import           CROE.Common.User
import           Polysemy

data ChatService (m :: * -> *) r where
  SendMessage :: User -> SendChatMessage -> ChatService r ()
  ReceiveMessage :: User -> MVar ReceiveChatMessage -> ChatService r ()

makeSem ''ChatService
