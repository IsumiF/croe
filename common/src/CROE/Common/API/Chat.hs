{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module CROE.Common.API.Chat
  ( API
  , module CROE.Common.Chat
  ) where

import           CROE.Common.API.WithTotal
import           CROE.Common.Chat
import           Data.Int
import           Servant.API

type API = "chat" :>
  ( APIMessages :<|> APIContactList :<|> APITotalUnreadCount :<|> APIMarkAsRead )

-- |来自某人的历史消息list，从新到旧排序
type APIMessages = "messages"
  :> QueryParam "from" Int64
  :> QueryParam "limit" Int
  :> QueryParam "offset" Int
  :> Get '[JSON] (WithTotal [ReceiveChatMessage])

type APIContactList = "contacts"
  :> QueryParam "limit" Int
  :> QueryParam "offset" Int
  :> Get '[JSON] (WithTotal [Contact])

type APITotalUnreadCount = "total_unread_count"
  :> Get '[JSON] Int

type APIMarkAsRead = "mark_as_read"
  :> ReqBody '[JSON] [Int64]
  :> Post '[JSON] ()
