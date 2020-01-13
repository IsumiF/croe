module CROE.Backend.IntTest.APISpec.Base
  ( setupSpec
  , tearDownSpec
  , Server(..)
  -- *reexport
  , client
  , runClientM
  ) where

import           Control.Concurrent  (ThreadId, forkIO, throwTo)
import           Control.Exception   (AsyncException (UserInterrupt))
import           Data.Maybe          (fromMaybe)
import           Network.HTTP.Client (defaultManagerSettings, newManager)
import           Network.Socket.Wait (wait)
import           Servant.Client
import           System.Environment  (lookupEnv)
import           Text.Read           (readMaybe)

import           CROE.Backend.Main   (CmdArgs (..), mainWithArgs)

data Server = Server
  { _server_clientEnv :: ClientEnv
  , _server_threadId  :: ThreadId
  }

setupSpec :: IO Server
setupSpec = do
    portStr <- lookupEnv "PORT"
    let port = fromMaybe 8080 (portStr >>= readMaybe)
    threadId <- forkIO $ mainWithArgs (CmdArgs "config/dev.json" port)
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
    baseUrl' = BaseUrl Http "127.0.0.1" port "api"
