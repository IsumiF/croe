module CROE.Backend.Redis.Class
  ( Redis(..)
  , publish
  , pubSub
  , Redis.PubSub
  , Redis.subscribe
  , Redis.unsubscribe
  , Redis.Message(..)
  , Redis.Reply(..)
  ) where

import           Data.ByteString (ByteString)
import qualified Database.Redis  as Redis
import           Polysemy

data Redis (m :: * -> *) r where
  Publish :: ByteString -- ^channel
          -> ByteString -- ^message
          -> Redis m (Either Redis.Reply Integer)
  PubSub :: Redis.PubSub
         -> (Redis.Message -> IO Redis.PubSub)
         -> Redis m ()

makeSem ''Redis
