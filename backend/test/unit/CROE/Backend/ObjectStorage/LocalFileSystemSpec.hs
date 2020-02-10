{-# LANGUAGE OverloadedStrings #-}

module CROE.Backend.ObjectStorage.LocalFileSystemSpec
  ( spec
  ) where

import           Data.Function                   ((&))
import           Data.Text                       (Text)
import           Polysemy
import           System.IO.Temp
import           Test.Hspec

import           CROE.Backend.Logger.Base        (runLoggerPure)
import           CROE.Backend.Logger.Class
import           CROE.Backend.ObjectStorage.Base

spec :: Spec
spec = around setupSuite $
    describe "putBytes and getBytes" $ do
      it "reads previously written data" $ \env -> do
        (logs, content) <- run' env $ do
          let key = "someDir/a.dat"
          ok <- putBytes key "hello world"
          embed $ ok `shouldBe` True
          getBytes key
        logs `shouldBe` []
        content `shouldBe` Just "hello world"
      it "rejects invalid key without log" $ \env -> do
        (logs, _) <- run' env $ do
          ok <- putBytes "/invalid" "hi"
          embed $ ok `shouldBe` False
        logs `shouldBe` []

setupSuite :: (Env -> IO ()) -> IO ()
setupSuite consumer = withSystemTempDirectory "croe.ObjectStorage" $ \dir -> do
    let env = newEnv (Config dir)
    consumer env

run' :: Env -> Sem '[ObjectStorage, Logger, Embed IO] a -> IO ([(LogLevel, Text)], a)
run' env app = app
    & runLocalFileSystem env
    & runLoggerPure
    & runM
