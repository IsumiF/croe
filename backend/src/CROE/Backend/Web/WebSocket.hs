{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE QuasiQuotes     #-}

module CROE.Backend.Web.WebSocket
  ( wsHandler
  ) where

import           Control.Lens
import           Control.Monad                   (forever)
import           Control.Monad.Except
import           CROE.Backend.Logger.Class
import           CROE.Backend.Service.Chat.Class
import           CROE.Common.User
import           CROE.Common.WebSocket
import           Data.String.Interpolate         (i)
import           Network.WebSockets.Connection
import           Polysemy
import           Polysemy.Async
import           Servant.Server
import           UnliftIO.Concurrent

type Constraint r = Members '[Embed IO, Logger, ChatService, Async] r

wsHandler :: Constraint r
          => User
          -> Connection
          -> ExceptT ServerError (Sem r) ()
wsHandler user conn = ExceptT $ wsHandler' user conn

wsHandler' :: forall r. Constraint r
           => User
           -> Connection
           -> Sem r (Either ServerError ())
wsHandler' user conn = do
    printLogt LevelInfo [i|user #{user ^. user_id} is connecting through websocket|]
    msgReceivedMVar :: MVar ChatMessage <- newEmptyMVar -- 当前用户接收到的消息
    void . async $ receiveMessage user msgReceivedMVar
    void . async . forever $ do
      msgReceived <- embed $ takeMVar msgReceivedMVar
      printLogt LevelInfo [i|msgReceived: #{msgReceived}|]
      embed $ sendTextData conn (renderWsMessage (WsMessageChat msgReceived))
    void . forever $ do
      msg :: Maybe WsMessage <- parseWsMessage <$> embed (receiveData conn)
      case msg of
        Nothing               -> printLogt LevelWarn "illegal websocket message format"
        Just msg' -> case msg' of
          WsMessageChat chatMsg -> sendMessage user chatMsg
    pure $ Right ()
