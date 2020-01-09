{-# LANGUAGE FlexibleContexts #-}

module CROE.Backend.Service.Auth
  ( checkBasicAuth
  ) where

import           Control.Monad.Reader
import           Crypto.KDF.BCrypt
import qualified Data.Text.Encoding         as T
import           Database.Persist.Types     (entityVal)
import           Servant

import           CROE.Backend.Persist.Class
import qualified CROE.Common.API            as Common

checkBasicAuth :: (MonadPersist backend m, ReadEntity User (ReaderT backend m))
               => BasicAuthData
               -> m (BasicAuthResult Common.User)
checkBasicAuth (BasicAuthData email password) = do
    entity' <- withConn $ getBy $ UniqueUserEmail (T.decodeUtf8 email)
    case entity' of
      Nothing     -> pure NoSuchUser
      Just entity -> pure $
        let user = entityVal entity
            hash = userHashedPassword (entityVal entity)
            isValid = validatePassword password hash
         in if isValid
            then Authorized $ Common.User (userEmail user) (userName user)
            else BadPassword
