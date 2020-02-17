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
  , TaskClient(..)
  , taskClient_newTask
  , taskClient_updateTask
  , taskClient_publishTask
  , taskClient_getTask
  ) where

import           Control.Concurrent        (ThreadId, forkIO, throwTo)
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
tearDownSpec server = throwTo threadId UserInterrupt
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
        let _taskClient_newTask
              :<|> _taskClient_updateTask
              :<|> _taskClient_publishTask
              :<|> _taskClient_getTask
                = protected user
            _protectedClient_task = TaskClient{..}
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
  , _userClient_putProfile    :: BasicAuthData -> User -> ClientM NoContent
  , _userClient_getProfile    :: BasicAuthData -> ClientM User
  }

data ProtectedClient = ProtectedClient
  { _protectedClient_task :: TaskClient
  }

data TaskClient = TaskClient
  { _taskClient_newTask     :: NewTaskRequest -> ClientM Int64
  , _taskClient_updateTask  :: Int64 -> NewTaskRequest -> ClientM NoContent
  , _taskClient_publishTask :: Int64 -> ClientM NoContent
  , _taskClient_getTask     :: Int64 -> ClientM TaskDetail
  }

makeLenses ''Client
makeLenses ''UserClient
makeLenses ''ProtectedClient
makeLenses ''TaskClient
