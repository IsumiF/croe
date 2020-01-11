module CROE.Frontend.Env.TH
  ( embedConfig
  ) where

import           Control.Monad.IO.Class
import qualified Data.ByteString        as BS
import           Data.FileEmbed         (bsToExp)
import           Data.Maybe
import           Language.Haskell.TH
import           System.Environment     (lookupEnv)

embedConfig :: FilePath -- ^config directory
            -> Q Exp
embedConfig dir = do
    envMaybe <- liftIO $ lookupEnv "ENV"
    let env = fromMaybe "dev" envMaybe
    bs <- liftIO $ BS.readFile (dir <> "/" <> env <> ".json")
    bsToExp bs
