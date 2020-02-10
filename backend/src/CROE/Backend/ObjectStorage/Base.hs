module CROE.Backend.ObjectStorage.Base
  ( Config(..)
  , Env
  , newEnv
  , runLocalFileSystem
  , module CROE.Backend.ObjectStorage.Class
  ) where

import           Control.Exception
import           Data.Aeson
import           Data.ByteString                  (ByteString)
import qualified Data.ByteString                  as BS
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           GHC.Generics                     (Generic)
import           Polysemy
import           System.Directory                 (createDirectoryIfMissing)
import           System.FilePath

import           CROE.Backend.Logger.Class
import           CROE.Backend.ObjectStorage.Class
import           CROE.Common.Util                 (aesonOptions)

newtype Config = Config
  { _config_baseDir :: FilePath
  } deriving (Show, Eq, Generic)

instance FromJSON Config where
  parseJSON = genericParseJSON aesonOptions

newtype Env = Env FilePath -- base dir

newEnv :: Config -> Env
newEnv (Config baseDir) = Env baseDir

runLocalFileSystem :: Members [Embed IO, Logger] r
                   => Env
                   -> Sem (ObjectStorage : r) a
                   -> Sem r a
runLocalFileSystem (Env baseDir) = interpret $ \case
    PutBytes key content ->
      if not (checkKey key)
      then pure False
      else do
        let filename = baseDir </> T.unpack key
            dir = takeDirectory filename
        result :: Either SomeException () <- embed $ try $ do
          createDirectoryIfMissing True dir
          BS.writeFile (baseDir </> T.unpack key) content
        case result of
          Left e  -> printLog LevelError (show e)
          Right _ -> pure ()
        pure True
    GetBytes key ->
      if not (checkKey key)
      then pure Nothing
      else do
        result :: Either SomeException ByteString <- embed $ try $
          BS.readFile (baseDir </> T.unpack key)
        case result of
          Left e  -> do
            printLog LevelError (show e)
            pure Nothing
          Right content -> pure (Just content)

checkKey :: Text -> Bool
checkKey key = isRelative (T.unpack key)
