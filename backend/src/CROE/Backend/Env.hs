{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}

module CROE.Backend.Env
  ( runApp
  , AppEffects
  , withEnv
  , Env
  , readConfig
  , Config
  ) where

import           Control.Monad.IO.Unlift
import           Data.Aeson
import           Data.Bifunctor
import           Data.ByteString                          (ByteString)
import qualified Data.ByteString.Lazy                     as LBS
import           Data.Function                            ((&))
import           Data.Text                                (Text)
import qualified Data.Text                                as T
import           GHC.Generics                             (Generic)
import           Polysemy
import           Polysemy.Async
import           Polysemy.Reader

import qualified CROE.Backend.Clock.Base                  as Clock
import           CROE.Backend.Clock.Class
import qualified CROE.Backend.Logger.Base                 as Logger
import           CROE.Backend.Logger.Class
import qualified CROE.Backend.Mail.Base                   as Mail
import qualified CROE.Backend.Mail.Class                  as Mail
import qualified CROE.Backend.ObjectStorage.Base          as ObjectStorage
import           CROE.Backend.ObjectStorage.Class
import qualified CROE.Backend.Persist.Base                as Persist
import qualified CROE.Backend.Persist.Class               as Persist
import           CROE.Backend.Random.Base
import qualified CROE.Backend.Redis.Base                  as Redis
import qualified CROE.Backend.Redis.Class                 as Redis
import qualified CROE.Backend.Service.Auth.Base           as AuthService
import           CROE.Backend.Service.Auth.Class
import           CROE.Backend.Service.Chat.Base
import           CROE.Backend.Service.Chat.Class
import qualified CROE.Backend.Service.Elasticsearch.Base  as EsService
import           CROE.Backend.Service.Elasticsearch.Class
import           CROE.Common.Util                         (aesonOptions)

data Env = Env
  { _env_persist              :: Persist.Env
  , _env_logger               :: Logger.Env
  , _env_mail                 :: Mail.Env
  , _env_authService          :: AuthService.Env
  , _env_objectStorage        :: ObjectStorage.Env
  , _env_elasticsearchService :: EsService.Config
  , _env_redis                :: Redis.Env
  }

data Config = Config
  { _config_persist              :: Persist.Config
  , _config_logger               :: Logger.Config
  , _config_mail                 :: Mail.Config
  , _config_authService          :: AuthService.Config
  , _config_objectStorage        :: ObjectStorage.Config
  , _config_elasticsearchService :: EsService.Config
  , _config_redis                :: Redis.Config
  } deriving Generic

readConfig :: ByteString -> Either Text Config
readConfig = first T.pack . eitherDecode' . LBS.fromStrict

instance FromJSON Config where
  parseJSON = genericParseJSON aesonOptions

withEnv :: Config -> (Env -> IO a) -> IO a
withEnv c f =
    Logger.withEnv (_config_logger c) $ \_env_logger ->
    Persist.withEnv (_config_persist c) _env_logger $ \_env_persist ->
    Redis.withEnv (_config_redis c) $ \_env_redis -> do
      let _env_mail = Mail.newEnv (_config_mail c)
      _env_authService <- AuthService.newEnv (_config_authService c)
      let _env_objectStorage = ObjectStorage.newEnv (_config_objectStorage c)
          _env_elasticsearchService = _config_elasticsearchService c
          env = Env{..}
      f env

instance MonadUnliftIO (Sem AppEffects) where
  askUnliftIO = do
    env <- ask
    pure (UnliftIO (runApp env))

type AppEffects =
  '[ ChatService
   , AuthService
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
   , Persist.ReadEntity Persist.SchoolCampus
   , Persist.WriteEntity Persist.SchoolCampus
   , Persist.ReadEntity Persist.Task
   , Persist.WriteEntity Persist.Task
   , Persist.ReadEntity Persist.Review
   , Persist.WriteEntity Persist.Review
   , Persist.ReadEntity Persist.ChatMessage
   , Persist.WriteEntity Persist.ChatMessage
   , Mail.Client
   , Elasticsearch
   , Redis.Redis
   , ObjectStorage
   , Clock
   , RandomService
   , Logger
   , Async
   , Reader Env
   , Embed IO
   ]

runApp :: Env -> Sem AppEffects a -> IO a
runApp env@Env{..} app = app
    & runChatService
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
    & Persist.runReadEntity
    & Persist.runWriteEntity
    & Persist.runReadEntity
    & Persist.runWriteEntity
    & Persist.runReadEntity
    & Persist.runWriteEntity
    & Mail.runClient _env_mail
    & EsService.runElasticsearch _env_elasticsearchService
    & Redis.runRedis _env_redis
    & ObjectStorage.runLocalFileSystem _env_objectStorage
    & Clock.runClock
    & runRandomService
    & Logger.runLogger _env_logger
    & asyncToIO
    & runReader env
    & runM
