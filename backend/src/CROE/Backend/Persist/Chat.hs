{-# LANGUAGE QuasiQuotes #-}

module CROE.Backend.Persist.Chat
  ( ChatRepo(..)
  , contactList
  , bulkUpdateStatus
  , runChatRepo
  ) where

import           CROE.Backend.Persist.Internal (Connection, RawSqlRunner,
                                                rawExecuteCount, rawSql)
import           CROE.Backend.Persist.Types
import qualified CROE.Common.Chat              as Common
import           CROE.Common.Util              (safeHead)
import           Data.Bifunctor                (second)
import           Data.Either                   (rights)
import           Data.Functor                  (void)
import           Data.String.Interpolate       (i)
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Data.Time
import           Database.Persist.Sql          (Single, toPersistValue,
                                                unSingle)
import           Database.Persist.Types
import           Polysemy

data ChatRepo (m :: * -> *) a where
  ContactList :: Connection
              -> UserId
              -> Int -- ^limit
              -> Int -- ^offset
              -> ChatRepo m ([(Entity User, Int)], Int) -- ^users with total
  BulkUpdateStatus :: Connection
                   -> UserId -- ^to user
                   -> [ChatMessageId] -- ^message ids
                   -> ChatMessageStatus
                   -> ChatRepo m ()

makeSem ''ChatRepo

runChatRepo :: Members '[RawSqlRunner] r
            => Sem (ChatRepo : r) a
            -> Sem r a
runChatRepo = interpret $ \case
  ContactList conn userId limit offset -> do
    totalSingle <- rawSql conn
      [i|
        SELECT COUNT(DISTINCT `from`)
        FROM chat_message
        WHERE chat_message.`from` = ? OR chat_message.`to` = ?
      |]
      [ toPersistValue userId, toPersistValue userId
      ]
    let total = maybe 0 unSingle (safeHead totalSingle)
    result :: [(Entity User, Single Int, Single UTCTime)] <- rawSql conn
      [i|
        SELECT ??,
          SUM(IF(stat = ? AND chat_message.`to` = ?, 1, 0)) AS cnt,
          MAX(chat_message.time) AS max_time
        FROM chat_message
        INNER JOIN user ON user.id = chat_message.`from` AND chat_message.`from` <> ?
        WHERE chat_message.`from` = ? OR chat_message.to = ?
        GROUP BY user.id
        ORDER BY cnt DESC, max_time DESC
        LIMIT ?
        OFFSET ?
      |]
      [ toPersistValue (ChatMessageStatus Common.CmsUnread)
      , toPersistValue userId
      , toPersistValue userId
      , toPersistValue userId
      , toPersistValue userId
      , toPersistValue limit
      , toPersistValue offset
      ]
    let result' = fmap dropThird result
    pure (fmap (second unSingle) result', total)
  BulkUpdateStatus conn toUser msgIds status ->
    void $ rawExecuteCount conn
      [i|
      UPDATE chat_message
      SET `stat` = ?
      WHERE `to` = ? AND `id` IN (#{marshalIds msgIds})
      |]
      [ toPersistValue status
      , toPersistValue toUser
      ]

dropThird :: (a, b, c) -> (a, b)
dropThird (x, y, _) = (x, y)

marshalIds :: [ChatMessageId] -> Text
marshalIds = T.intercalate "," . rights . fmap (fromPersistValueText . toPersistValue)
