{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}

module CROE.Backend.Env
  ( runApp
  , AppEffects
  , withEnv
  , Env
  , readConfig
  , Config
  ) where

import           Data.Aeson
import           Data.Bifunctor
import           Data.ByteString                  (ByteString)
import qualified Data.ByteString.Lazy             as LBS
import           Data.Function                    ((&))
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           GHC.Generics                     (Generic)
import           Polysemy

import qualified CROE.Backend.Clock.Base          as Clock
import           CROE.Backend.Clock.Class
import qualified CROE.Backend.Logger.Base         as Logger
import           CROE.Backend.Logger.Class
import qualified CROE.Backend.Mail.Base           as Mail
import qualified CROE.Backend.Mail.Class          as Mail
import qualified CROE.Backend.ObjectStorage.Base  as ObjectStorage
import           CROE.Backend.ObjectStorage.Class
import qualified CROE.Backend.Persist.Base        as Persist
import qualified CROE.Backend.Persist.Class       as Persist
import           CROE.Backend.Random.Base
import qualified CROE.Backend.Service.Auth.Base   as AuthService
import           CROE.Backend.Service.Auth.Class
import           CROE.Common.Util                 (aesonOptions)

data Env = Env
  { _env_persist       :: Persist.Env
  , _env_logger        :: Logger.Env
  , _env_mail          :: Mail.Env
  , _env_authService   :: AuthService.Env
  , _env_objectStorage :: ObjectStorage.Env
  }

data Config = Config
  { _config_persist       :: Persist.Config
  , _config_logger        :: Logger.Config
  , _config_mail          :: Mail.Config
  , _config_authService   :: AuthService.Config
  , _config_objectStorage :: ObjectStorage.Config
  } deriving Generic

readConfig :: ByteString -> Either Text Config
readConfig = first T.pack . eitherDecode' . LBS.fromStrict

instance FromJSON Config where
  parseJSON = genericParseJSON aesonOptions

withEnv :: Config -> (Env -> IO a) -> IO a
withEnv c f =
    Logger.withEnv (_config_logger c) $ \_env_logger ->
    Persist.withEnv (_config_persist c) _env_logger $ \_env_persist -> do
      let _env_mail = Mail.newEnv (_config_mail c)
      _env_authService <- AuthService.newEnv (_config_authService c)
      let _env_objectStorage = ObjectStorage.newEnv (_config_objectStorage c)
          env = Env{..}
      f env

type AppEffects =
  '[ AuthService
   , Persist.ConnectionPool
   , Persist.Transactional
   , Persist.ReadEntity Persist.User
   , Persist.WriteEntity Persist.User
   , Persist.ReadEntity Persist.UserRegistry
   , Persist.WriteEntity Persist.UserRegistry
   , Persist.ReadEntity Persist.School
   , Persist.WriteEntity Persist.School
   , Persist.ReadEntity Persist.SchoolDomain
   , Persist.WriteEntity Persist.SchoolDomain
   , Persist.ReadEntity Persist.Task
   , Persist.WriteEntity Persist.Task
   , Mail.Client
   , ObjectStorage
   , Clock
   , RandomService
   , Logger
   , Embed IO
   ]

runApp :: Env -> Sem AppEffects a -> IO a
runApp Env{..} app = app
    & AuthService.runService _env_authService
    & Persist.runConnectionPool _env_persist
    & Persist.runTransactional
    & Persist.runReadEntity
    & Persist.runWriteEntity
    & Persist.runReadEntity
    & Persist.runWriteEntity
    & Persist.runReadEntity
    & Persist.runWriteEntity
    & Persist.runReadEntity
    & Persist.runWriteEntity
    & Persist.runReadEntity
    & Persist.runWriteEntity
    & Mail.runClient _env_mail
    & ObjectStorage.runLocalFileSystem _env_objectStorage
    & Clock.runClock
    & runRandomService
    & Logger.runLogger _env_logger
    & runM
