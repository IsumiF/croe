{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

module CROE.Backend.Service.Auth.Base
  ( Config
  , Env
  , newEnv
  , runService
  ) where

import qualified Crypto.KDF.BCrypt               as BCrypt
import           Data.Aeson
import           Data.Cache                      (Cache)
import qualified Data.Cache                      as Cache
import           Data.Hashable
import qualified Data.Text.Encoding              as T
import           GHC.Generics                    (Generic)
import           Polysemy
import           Servant
import           System.Clock

import           CROE.Backend.Persist.Class
import           CROE.Backend.Service.Auth.Class
import qualified CROE.Common.API.User            as Common
import           CROE.Common.Util                (aesonOptions)

newtype Config = Config
  { _config_cacheExpiration :: Integer -- ^cache expiration time, in seconds
  } deriving (Show, Eq, Generic)

instance FromJSON Config where
  parseJSON = genericParseJSON aesonOptions

newtype Env = Env (Cache CacheKey (BasicAuthResult Common.User))

newEnv :: Config -> IO Env
newEnv (Config cacheExpiration) = do
    cache <- Cache.newCache (Just (TimeSpec (fromInteger cacheExpiration) 0))
    pure (Env cache)

newtype CacheKey = CacheKey BasicAuthData

instance Eq CacheKey where
  CacheKey (BasicAuthData n1 p1) == CacheKey (BasicAuthData n2 p2) =
    n1 == n2 && p1 == p2

instance Hashable CacheKey where
  hashWithSalt s (CacheKey (BasicAuthData n p)) =
    s `hashWithSalt` n `hashWithSalt` p

runService :: Members [Embed IO, ConnectionPool, ReadEntity User] r
           => Env
           -> Sem (AuthService : r) a
           -> Sem r a
runService (Env cache) = interpret $ \case
    CheckBasicAuth authData@(BasicAuthData email password) -> do
      cacheResult <- embed $ Cache.lookup cache (CacheKey authData)
      case cacheResult of
        Just cacheResult' -> pure cacheResult'
        Nothing -> do
          entity' <- withConn $ \conn -> getBy conn $ UniqueUserEmail (T.decodeUtf8 email)
          case entity' of
            Nothing     -> pure NoSuchUser
            Just entity -> do
              let hash' = userHashedPassword (entityVal entity)
                  isValid = BCrypt.validatePassword password hash'
                  result =
                    if isValid
                    then Authorized $ userToCommon entity
                    else BadPassword
              embed $ Cache.insert cache (CacheKey authData) result
              pure result
    HashPassword clearText -> embed $ BCrypt.hashPassword 12 clearText
    InvalidateCache authData ->
      embed $ Cache.delete cache (CacheKey authData)
