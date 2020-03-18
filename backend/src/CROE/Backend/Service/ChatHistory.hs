module CROE.Backend.Service.ChatHistory
  ( messageList
  , contactList
  , totalUnreadCount
  , markAsRead
  ) where

import           Control.Lens
import           Control.Monad.Trans.Class    (lift)
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Maybe
import           CROE.Backend.Logger.Class
import qualified CROE.Backend.Persist.Chat    as Persist
import qualified CROE.Backend.Persist.Class   as Persist
import           CROE.Backend.Persist.Types
import           CROE.Backend.Service.Convert
import           CROE.Common.API.WithTotal
import qualified CROE.Common.Chat             as Common
import qualified CROE.Common.User             as Common
import           Data.Coerce
import           Data.Int
import           Database.Persist             ((==.))
import           Database.Persist.Sql         (toSqlKey)
import           Database.Persist.Types
import           Polysemy
import           Servant.API                  (NoContent(..))
import           Servant.Server

messageList :: Members '[ Logger
                        , Persist.ConnectionPool
                        , Persist.ReadEntity ChatMessage
                        ] r
            => Common.User
            -> Maybe Int64
            -> Maybe Int
            -> Maybe Int
            -> Sem r (Either ServerError (WithTotal [Common.ReceiveChatMessage]))
messageList me fromUserIdMaybe limitMaybe offsetMaybe = runExceptT $ do
    fromUserId <- maybeToExceptT err400 . MaybeT . pure $ fromUserIdMaybe
    limit <- maybeToExceptT err400 . MaybeT . pure $ limitMaybe
    offset <- maybeToExceptT err400 . MaybeT . pure $ offsetMaybe
    let sqlFilter = [ChatMessageFrom ==. toSqlKey fromUserId, ChatMessageTo ==. toSqlKey myId]
    (msgs, total) <- lift . Persist.withConn $ \conn -> do
      total <- Persist.count conn sqlFilter
      msgs <- Persist.selectList conn sqlFilter
        [LimitTo limit, OffsetBy offset, Desc ChatMessageTime]
      pure (msgs, total)
    let result = WithTotal (fmap chatMessageToCommon msgs) total
    pure result
  where
    myId = me ^. Common.user_id

contactList :: Members '[ Persist.ConnectionPool
                        , Persist.ChatRepo
                        ] r
            => Common.User
            -> Maybe Int -- ^limit
            -> Maybe Int -- ^offset
            -> Sem r (Either ServerError (WithTotal [Common.Contact]))
contactList me limitMaybe offsetMaybe = runExceptT $ do
    limit <- maybeToExceptT err400 . MaybeT . pure $ limitMaybe
    offset <- maybeToExceptT err400 . MaybeT . pure $ offsetMaybe
    (result, total) <- lift $ Persist.withConn $ \conn ->
      Persist.contactList conn (toSqlKey myId) limit offset
    let contacts = fmap userToContact result
    pure $ WithTotal contacts total
  where
    myId = me ^. Common.user_id

userToContact :: (Entity User, Int) -> Common.Contact
userToContact (entityUser, unreadCount) = Common.Contact
    { _contact_user = userToCommon entityUser
    , _contact_unreadCount = unreadCount
    }

totalUnreadCount :: Members '[ Persist.ConnectionPool
                             , Persist.ReadEntity ChatMessage
                             ] r
                 => Common.User
                 -> Sem r (Either ServerError Int)
totalUnreadCount me = do
    cnt <- Persist.withConn $ \conn ->
      Persist.count conn [ChatMessageTo ==. myId, ChatMessageStat ==. coerce Common.CmsUnread]
    pure $ Right cnt
  where
    myId = toSqlKey (me ^. Common.user_id)

markAsRead :: Members '[ Persist.ConnectionPool
                       , Persist.ChatRepo
                       ] r
           => Common.User
           -> [Int64]
           -> Sem r (Either ServerError NoContent)
markAsRead me msgIds = do
    Persist.withConn $ \conn ->
      Persist.bulkUpdateStatus conn
        (toSqlKey $ me ^. Common.user_id)
        (fmap toSqlKey msgIds)
        (coerce Common.CmsRead)
    pure $ Right NoContent
