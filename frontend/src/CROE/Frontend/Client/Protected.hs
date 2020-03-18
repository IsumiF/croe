{-# LANGUAGE StrictData      #-}
{-# LANGUAGE TemplateHaskell #-}

module CROE.Frontend.Client.Protected
  ( ProtectedClient(..)
  , protectedClient_task
  , protectedClient_school
  , protectedClient_chat
  , TaskClient(..)
  , taskClient_new
  , taskClient_update
  , taskClient_changeStatus
  , taskClient_get
  , taskClient_search
  , taskClient_reindex
  , SchoolClient(..)
  , schoolClient_get
  , ChatClient(..)
  , chatClient_messages
  , chatClient_contactList
  , chatClient_totalUnreadCount
  , chatClient_markAsRead
  ) where

import           Control.Lens
import           Data.Int
import           Data.Text
import           Reflex.Dom
import           Servant.API
import           Servant.Reflex

import           CROE.Common.API.Chat
import           CROE.Common.API.School
import           CROE.Common.API.Task
import           CROE.Common.API.WithTotal

data ProtectedClient t m = ProtectedClient
  { _protectedClient_task   :: TaskClient t m
  , _protectedClient_school :: SchoolClient t m
  , _protectedClient_chat   :: ChatClient t m
  }

data TaskClient t m = TaskClient
  { _taskClient_new :: Dynamic t (Either Text NewTaskRequest)
                    -> Event t ()
                    -> m (Event t (ReqResult () Int64))
  , _taskClient_update :: Dynamic t (Either Text Int64)
                       -> Dynamic t (Either Text NewTaskRequest)
                       -> Event t ()
                       -> m (Event t (ReqResult () NoContent))
  , _taskClient_changeStatus :: Dynamic t (Either Text Int64)
                             -> Dynamic t (QParam TaskAction)
                             -> Event t ()
                             -> m (Event t (ReqResult () NoContent))
  , _taskClient_get :: Dynamic t (Either Text Int64)
                    -> Event t ()
                    -> m (Event t (ReqResult () TaskDetail))
  , _taskClient_search :: Dynamic t (Either Text TaskQueryCondition)
                       -> Event t ()
                       -> m (Event t (ReqResult () TaskSearchResult))
  , _taskClient_reindex :: Event t ()
                        -> m (Event t (ReqResult () NoContent))
  }

newtype SchoolClient t m = SchoolClient
  { _schoolClient_get :: Event t ()
                      -> m (Event t (ReqResult () [School]))
  }

data ChatClient t m = ChatClient
  { _chatClient_messages :: Dynamic t (QParam Int64) -- from user id
                         -> Dynamic t (QParam Int) -- limit
                         -> Dynamic t (QParam Int) -- offset
                         -> Event t ()
                         -> m (Event t (ReqResult () (WithTotal [ReceiveChatMessage])))
  , _chatClient_contactList :: Dynamic t (QParam Int) -- limit
                            -> Dynamic t (QParam Int) -- offset
                            -> Event t ()
                            -> m (Event t (ReqResult () (WithTotal [Contact])))
  , _chatClient_totalUnreadCount :: Event t ()
                                 -> m (Event t (ReqResult () Int))
  , _chatClient_markAsRead :: Dynamic t (Either Text [Int64])
                           -> Event t ()
                           -> m (Event t (ReqResult () NoContent))
  }

makeLenses ''ProtectedClient
makeLenses ''TaskClient
makeLenses ''SchoolClient
makeLenses ''ChatClient
