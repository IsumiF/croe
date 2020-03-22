{-# LANGUAGE StrictData      #-}
{-# LANGUAGE TemplateHaskell #-}

module CROE.Backend.IntTest.APISpec.Base
  ( setupSpec
  , tearDownSpec
  , Server(..)
  , _FailureResponse
  , runClientM'
  , getStatusCode
  -- *Client Functions
  , servantClient
  , Client(..)
  , client_user
  , client_protected
  , UserClient(..)
  , userClient_applyCode
  , userClient_register
  , userClient_validateEmail
  , userClient_putProfile
  , userClient_getProfile
  , ProtectedClient(..)
  , protectedClient_task
  , protectedClient_school
  , protectedClient_chat
  , TaskClient(..)
  , taskClient_newTask
  , taskClient_updateTask
  , taskClient_updateStatus
  , taskClient_getTask
  , taskClient_search
  , taskClient_reindex
  , taskClient_addReview
  , SchoolClient(..)
  , schoolClient_get
  , ChatClient(..)
  , chatClient_messages
  , chatClient_contactList
  , chatClient_totalUnreadCount
  , chatClient_markAsRead
  ) where

import           Control.Concurrent        (ThreadId, forkIO, threadDelay,
                                            throwTo)
import           Control.Exception         (AsyncException (UserInterrupt))
import           Control.Lens
import           Data.Int
import           Data.Maybe                (fromMaybe)
import           Data.Proxy
import           Data.Text                 (Text)
import           Network.HTTP.Client       (defaultManagerSettings, newManager)
import           Network.HTTP.Types.Status (Status (..))
import           Network.Socket.Wait       (wait)
import           Servant.API
import           Servant.Client            hiding (Client)
import           System.Environment        (lookupEnv)
import           Text.Read                 (readMaybe)

import           CROE.Backend.Main         (CmdArgs (..), mainWithArgs)
import           CROE.Common.API
import           CROE.Common.API.Task      hiding (API)
import           CROE.Common.API.User      hiding (API)
import           CROE.Common.API.WithTotal
import           CROE.Common.Chat
import           CROE.Common.School

data Server = Server
  { _server_clientEnv :: ClientEnv
  , _server_threadId  :: ThreadId
  }

setupSpec :: IO Server
setupSpec = do
    portStr <- lookupEnv "PORT"
    env <- lookupEnv "ENV"
    let env' = fromMaybe "dev" env
        configFile = "config/" <> env' <> ".json"
        port = fromMaybe 8081 (portStr >>= readMaybe)
    threadId <- forkIO $ mainWithArgs (CmdArgs configFile port)
    wait "127.0.0.1" port
    clientEnv <- clientEnvFromPort port
    pure $ Server clientEnv threadId

tearDownSpec :: Server -> IO ()
tearDownSpec server = do
    -- wait for logs to output
    threadDelay (2000 * 1000)
    throwTo threadId UserInterrupt
  where
    threadId = _server_threadId server

clientEnvFromPort :: Int -> IO ClientEnv
clientEnvFromPort port = do
    manager' <- newManager defaultManagerSettings
    pure $ mkClientEnv manager' baseUrl'
  where
    baseUrl' = BaseUrl Http "127.0.0.1" port ""

makePrisms ''ClientError

-- |runClientM specialized for Server type
runClientM' :: Server
            -> ClientM a
            -> IO (Either ClientError a)
runClientM' server m =
    runClientM m (_server_clientEnv server)

getStatusCode :: Either ClientError a -> Maybe Int
getStatusCode =
    fmap (statusCode . responseStatusCode)
      . preview (_Left . _FailureResponse . _2)

servantClient :: Client
servantClient =
  let (userProtected :<|> _userClient_applyCode :<|> _userClient_register :<|> _userClient_validateEmail) :<|> protected = client (Proxy :: Proxy API)
      _userClient_putProfile user = let (f :<|> _) = userProtected user in f
      _userClient_getProfile user = let (_ :<|> g) = userProtected user in g
      _client_user = UserClient{..}

      _client_protected user =
        let ( _taskClient_newTask
              :<|> _taskClient_updateTask
              :<|> _taskClient_updateStatus
              :<|> _taskClient_getTask
              :<|> _taskClient_search
              :<|> _taskClient_reindex
              :<|> _taskClient_addReview
              )
              :<|> _schoolClient_get
              :<|> (
                _chatClient_messages :<|>
                _chatClient_contactList :<|>
                _chatClient_totalUnreadCount :<|>
                _chatClient_markAsRead
              )
                = protected user
            _protectedClient_task = TaskClient{..}
            _protectedClient_school = SchoolClient{..}
            _protectedClient_chat = ChatClient{..}

         in ProtectedClient{..}
   in Client{..}

data Client = Client
  { _client_user      :: UserClient
  , _client_protected :: BasicAuthData -> ProtectedClient
  }

data UserClient = UserClient
  { _userClient_applyCode     :: Maybe Text -> ClientM NoContent
  , _userClient_register      :: RegisterForm -> ClientM NoContent
  , _userClient_validateEmail :: EmailAddress -> ClientM Text
  , _userClient_putProfile    :: BasicAuthData -> PutProfileForm -> ClientM NoContent
  , _userClient_getProfile    :: BasicAuthData -> ClientM User
  }

data ProtectedClient = ProtectedClient
  { _protectedClient_task   :: TaskClient
  , _protectedClient_school :: SchoolClient
  , _protectedClient_chat   :: ChatClient
  }

data TaskClient = TaskClient
  { _taskClient_newTask      :: NewTaskRequest -> ClientM Int64
  , _taskClient_updateTask   :: Int64 -> NewTaskRequest -> ClientM NoContent
  , _taskClient_updateStatus :: Int64 -> Maybe TaskAction -> ClientM NoContent
  , _taskClient_getTask      :: Int64 -> ClientM TaskDetail
  , _taskClient_search       :: TaskQueryCondition -> ClientM TaskSearchResult
  , _taskClient_reindex      :: ClientM NoContent
  , _taskClient_addReview    :: Int64 -> TaskAddReview -> ClientM NoContent
  }

newtype SchoolClient = SchoolClient
  { _schoolClient_get :: ClientM [School]
  }

data ChatClient = ChatClient
  { _chatClient_messages :: Maybe Int64 -> Maybe Int -> Maybe Int -> ClientM (WithTotal [ReceiveChatMessage])
  , _chatClient_contactList :: Maybe Int -> Maybe Int -> ClientM (WithTotal [Contact])
  , _chatClient_totalUnreadCount :: ClientM Int
  , _chatClient_markAsRead :: [Int64] -> ClientM NoContent
  }

makeLenses ''Client
makeLenses ''UserClient
makeLenses ''ProtectedClient
makeLenses ''TaskClient
makeLenses ''SchoolClient
makeLenses ''ChatClient
