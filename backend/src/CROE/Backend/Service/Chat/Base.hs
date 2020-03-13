{-# LANGUAGE QuasiQuotes #-}

module CROE.Backend.Service.Chat.Base
  ( runChatService
  -- *implementation
  , sendMessageImpl
  , receiveMessageImpl
  ) where

import           Control.Concurrent.MVar
import           Control.Lens
import           Control.Monad                   (forever)
import           CROE.Backend.Logger.Class
import qualified CROE.Backend.Persist.Class      as Persist
import           CROE.Backend.Persist.Types
import           CROE.Backend.Redis.Class        (Redis)
import qualified CROE.Backend.Redis.Class        as Redis
import           CROE.Backend.Service.Chat.Class
import qualified CROE.Common.User                as Common
import qualified CROE.Common.WebSocket           as Common
import           Data.Aeson                      (eitherDecode, encode)
import           Data.ByteString                 (ByteString)
import qualified Data.ByteString.Lazy            as LBS
import           Data.Functor                    (void)
import           Data.Int
import           Data.String.Interpolate         (i)
import           Data.Text
import           Database.Persist.Sql            (toSqlKey)
import           Polysemy
import           Polysemy.Async

runChatService :: ( Members '[ Logger
                             , Redis
                             , Persist.ConnectionPool
                             , Persist.WriteEntity ChatMessage
                             , Embed IO
                             , Async
                             ] r
                  )
               => Sem (ChatService : r) a
               -> Sem r a
runChatService = interpret $ \case
  SendMessage user msg -> sendMessageImpl user msg
  ReceiveMessage user msgMVar -> receiveMessageImpl user msgMVar

sendMessageImpl :: Members '[ Logger
                            , Redis
                            , Persist.ConnectionPool
                            , Persist.WriteEntity ChatMessage
                            ] r
                => Common.User
                -> Common.ChatMessage
                -> Sem r ()
sendMessageImpl user msg =
    if user ^. Common.user_id /= msg ^. Common.chatMessage_from
    then printLogt LevelWarn [i|user #{user ^. Common.user_id} trying to send message as #{msg ^. Common.chatMessage_from}|]
    else do
      printLogt LevelInfo [i|send message #{msg}|]
      -- send to redis
      reply <- Redis.publish (channelForUser toId) (LBS.toStrict (encode msg))
      case reply of
        Left (Redis.Error errMsg) -> printLogt LevelError [i|send message failed, reason: #{errMsg}|]
        _                         -> pure ()
      -- save to mysql
      let msg' = chatMessageFromCommon msg
      Persist.withConn $ \conn -> Persist.insert_ conn msg'
  where
    toId = Common._chatMessage_to msg

receiveMessageImpl :: Members '[Redis, Embed IO, Logger, Async] r
                   => Common.User
                   -> MVar Common.ChatMessage
                   -> Sem r ()
receiveMessageImpl user msgMVar = do
    logMVar :: MVar (Maybe Text) <- embed newEmptyMVar
    r <- async $ forever $ do
      logMaybe <- embed $ takeMVar logMVar
      case logMaybe of
        Nothing   -> pure ()
        Just log' -> printLogt LevelError log'
    Redis.pubSub (Redis.subscribe [channel]) $ \redisMessage -> do
      case redisMessage of
        Redis.Message _ rawMessage ->
          case eitherDecode (LBS.fromStrict rawMessage) of
            Left errMsg ->
              putMVar logMVar $
                Just [i|fail to decode chat message received from redis, reason: #{errMsg}, data: #{rawMessage}|]
            Right msg -> putMVar msgMVar msg >> putMVar logMVar Nothing
        msg@Redis.PMessage{} -> putMVar logMVar $ Just
          [i|unexpected type of message received from redis, data: #{msg}|]
      pure mempty
    void $ await r
  where
    currentUserId = user ^. Common.user_id
    channel = channelForUser currentUserId

chatMessageFromCommon :: Common.ChatMessage -> ChatMessage
chatMessageFromCommon Common.ChatMessage{..} = ChatMessage
  { chatMessageFrom = toSqlKey _chatMessage_from
  , chatMessageTo = toSqlKey _chatMessage_to
  , chatMessageBody = _chatMessage_body
  }

channelForUser :: Int64 -> ByteString
channelForUser uid = [i|chat.to.#{uid}|]
