module CROE.Backend.Redis.Base
  ( Config
  , Env
  , withEnv
  , runRedis
  ) where

import           Control.Monad.IO.Class
import           Control.Monad.IO.Unlift
import           CROE.Backend.Redis.Class
import           CROE.Common.Util         (aesonOptions)
import           Data.Aeson
import           Data.Text                (Text)
import qualified Data.Text.Encoding       as T
import qualified Database.Redis           as Redis
import           GHC.Generics
import           Polysemy

data Config = Config
  { _config_host     :: String
  , _config_port     :: Int
  , _config_password :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON Config where
  parseJSON = genericParseJSON aesonOptions

newtype Env = Env Redis.Connection

withEnv :: (MonadIO m, MonadUnliftIO m) => Config -> (Env -> m a) -> m a
withEnv Config{..} f = do
    runInIO <- askRunInIO
    liftIO $ Redis.withCheckedConnect
      Redis.defaultConnectInfo
        { Redis.connectHost = _config_host
        , Redis.connectPort = Redis.PortNumber (fromIntegral _config_port)
        , Redis.connectAuth = Just (T.encodeUtf8 _config_password)
        } (runInIO . f . Env)

runRedis :: Members '[Embed IO] r
         => Env
         -> Sem (Redis : r) a
         -> Sem r a
runRedis (Env conn) = interpret $ \case
  Publish channel message ->
    embed . Redis.runRedis conn $ Redis.publish channel message
  PubSub p f -> embed . Redis.runRedis conn $ Redis.pubSub p f
