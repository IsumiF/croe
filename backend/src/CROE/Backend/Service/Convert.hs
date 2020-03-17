module CROE.Backend.Service.Convert
  ( userToCommon
  , chatMessageToCommon
  ) where

import           CROE.Backend.Persist.Types
import qualified CROE.Common.Chat           as Common
import qualified CROE.Common.User           as Common
import           Data.Coerce
import           Database.Persist.Sql       (fromSqlKey)
import           Database.Persist.Types

userToCommon :: Entity User -> Common.User
userToCommon (Entity userId user) = Common.User
    { Common._user_id = fromSqlKey userId
    , Common._user_name = userName user
    , Common._user_email = userEmail user
    , Common._user_role = coerce (userRole user)
    }

chatMessageToCommon :: Entity ChatMessage -> Common.ReceiveChatMessage
chatMessageToCommon (Entity msgId msg) =
    Common.ReceiveChatMessage
      { Common._receiveChatMessage_id = fromSqlKey msgId
      , Common._receiveChatMessage_from = fromSqlKey (chatMessageFrom msg)
      , Common._receiveChatMessage_body = chatMessageBody msg
      , Common._receiveChatMessage_time = chatMessageTime msg
      , Common._receiveChatMessage_status = coerce (chatMessageStat msg)
      }
