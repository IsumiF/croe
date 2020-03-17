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
import           CROE.Backend.Clock.Class
import           CROE.Backend.Logger.Class
import qualified CROE.Backend.Persist.Class      as Persist
import           CROE.Backend.Persist.Types
import           CROE.Backend.Redis.Class        (Redis)
import qualified CROE.Backend.Redis.Class        as Redis
import           CROE.Backend.Service.Chat.Class
import qualified CROE.Common.Chat                as Common
import qualified CROE.Common.User                as Common
import           Data.Aeson                      (eitherDecode, encode)
import           Data.ByteString                 (ByteString)
import qualified Data.ByteString.Lazy            as LBS
import           Data.Coerce
import           Data.Functor                    (void)
import           Data.Int
import           Data.String.Interpolate         (i)
import           Data.Text
import           Database.Persist.Sql            (fromSqlKey, toSqlKey)
import           Polysemy
import           Polysemy.Async

runChatService :: ( Members '[ Logger
                             , Redis
                             , Persist.ConnectionPool
                             , Persist.WriteEntity ChatMessage
                             , Embed IO
                             , Async
                             , Clock
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
                            , Clock
                            ] r
                => Common.User
                -> Common.SendChatMessage
                -> Sem r ()
sendMessageImpl user msg = do
    printLogt LevelInfo [i|send message #{msg}|]
    now <- currentTimeUTC
    let msg' = ChatMessage
          (toSqlKey (user ^. Common.user_id))
          (toSqlKey toId)
          now
          (msg ^. Common.sendChatMessage_body)
          (coerce Common.CmsUnread)
    -- save to mysql
    msgId <- Persist.withConn $ \conn -> Persist.insert conn msg'
    -- send to redis
    reply <- Redis.publish (channelForUser toId) (LBS.toStrict (encode (msgId, msg')))
    case reply of
      Left (Redis.Error errMsg) -> printLogt LevelError [i|send message failed, reason: #{errMsg}|]
      _                         -> pure ()
  where
    toId = msg ^. Common.sendChatMessage_to

receiveMessageImpl :: Members '[Redis, Embed IO, Logger, Async] r
                   => Common.User
                   -> MVar Common.ReceiveChatMessage
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
            Right ((msgId, msg) :: (ChatMessageId, ChatMessage)) -> do
              let receiveMsg = Common.ReceiveChatMessage
                    { Common._receiveChatMessage_id = fromSqlKey msgId
                    , Common._receiveChatMessage_from = fromSqlKey (chatMessageFrom msg)
                    , Common._receiveChatMessage_body = chatMessageBody msg
                    , Common._receiveChatMessage_time = chatMessageTime msg
                    , Common._receiveChatMessage_status = coerce (chatMessageStat msg)
                    }
              putMVar msgMVar receiveMsg
              putMVar logMVar Nothing
        msg@Redis.PMessage{} -> putMVar logMVar $ Just
          [i|unexpected type of message received from redis, data: #{msg}|]
      pure mempty
    void $ await r
  where
    currentUserId = user ^. Common.user_id
    channel = channelForUser currentUserId

channelForUser :: Int64 -> ByteString
channelForUser uid = [i|chat.to.#{uid}|]
