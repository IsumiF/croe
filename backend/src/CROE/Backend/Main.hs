{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module CROE.Backend.Main
  ( main
  , mainWithArgs
  , CmdArgs(..)
  ) where

import qualified Data.ByteString          as BS
import qualified Data.Text.IO             as T
import           Network.Wai.Handler.Warp (Port, runEnv)
import           Options.Applicative

import           CROE.Backend.Env
import           CROE.Backend.Web

main :: IO ()
main = mainWithArgs =<< execParser opts
  where
    opts = info (parser <**> helper)
      ( fullDesc
     <> progDesc "Server for CROE"
      )

data CmdArgs = CmdArgs
  { confFile :: FilePath
  , confPort :: Port
  }

parser :: Parser CmdArgs
parser = CmdArgs
  <$> strOption
      ( long "conf-file"
     <> short 'c'
     <> metavar "CONF_FILE"
     <> help "path of config file"
     <> showDefault
     <> value "config/dev.json"
      )
  <*> option auto
      ( long "port"
     <> short 'p'
     <> metavar "PORT"
     <> help "port to run the service on"
     <> showDefault
     <> value 8080
      )

mainWithArgs :: CmdArgs -> IO ()
mainWithArgs CmdArgs{..} = do
    configBytes <- BS.readFile confFile
    case readConfig configBytes of
      Left errMsg  -> T.putStrLn $ "config file format error: " <> errMsg
      Right config -> withEnv config $ \env -> do
        let webApp = mkWebApp env
        runEnv confPort webApp
