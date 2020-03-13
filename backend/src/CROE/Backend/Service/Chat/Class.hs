module CROE.Backend.Service.Chat.Class
  ( ChatService(..)
  , sendMessage
  , receiveMessage
  ) where

import           Control.Concurrent.MVar
import           CROE.Common.User
import           CROE.Common.WebSocket   (ChatMessage (..))
import           Polysemy

data ChatService (m :: * -> *) r where
  SendMessage :: User -> ChatMessage -> ChatService r ()
  ReceiveMessage :: User -> MVar ChatMessage -> ChatService r ()

makeSem ''ChatService
