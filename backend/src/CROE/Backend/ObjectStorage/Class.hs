module CROE.Backend.ObjectStorage.Class
  ( ObjectStorage(..)
  , getBytes
  , putBytes
  ) where

import           Data.ByteString (ByteString)
import           Data.Text       (Text)
import           Polysemy

data ObjectStorage (m :: * -> *) r where
  PutBytes :: Text -- ^key
           -> ByteString -- ^content
           -> ObjectStorage m Bool
  GetBytes :: Text -- ^key
           -> ObjectStorage m (Maybe ByteString)

makeSem ''ObjectStorage
