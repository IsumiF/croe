{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators     #-}

module CROE.Backend.Web.ApiHandler
  ( apiHandler
  ) where

import           Control.Monad.Except
import           Polysemy
import           Servant

import           CROE.Backend.Env
import qualified CROE.Backend.Service.ChatHistory as ChatHistory
import qualified CROE.Backend.Service.School      as School
import qualified CROE.Backend.Service.Task        as Task
import qualified CROE.Backend.Service.User        as User
import           CROE.Common.API

apiHandler :: ServerT API (ExceptT ServerError (Sem AppEffects))
apiHandler =
    ((\user -> ExceptT . User.putProfile user
    :<|> ExceptT (User.getProfile user)
    )
    :<|> ExceptT . User.applyCode
    :<|> ExceptT . User.register
    :<|> ExceptT . User.validateEmail
    ) :<|> (
      \user ->
        ( ExceptT . Task.newTask user
        :<|> (\a b -> ExceptT $ Task.updateTask user a b)
        :<|> (\a b -> ExceptT $ Task.changeStatus user a b)
        :<|> ExceptT . Task.getTask user
        :<|> ExceptT . Task.searchTask user
        :<|> ExceptT (Task.reindex user)
        :<|> (\a b -> ExceptT $ Task.taskAddReview user a b)
        ) :<|>
          ExceptT (School.getSchoolList user)
        :<|> ( (\a b c -> ExceptT $ ChatHistory.messageList user a b c)
          :<|> (\a b -> ExceptT $ ChatHistory.contactList user a b)
          :<|> (ExceptT (ChatHistory.totalUnreadCount user))
          :<|> (ExceptT . ChatHistory.markAsRead user)
        )
    )
