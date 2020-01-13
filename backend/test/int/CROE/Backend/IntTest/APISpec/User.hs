{-# LANGUAGE OverloadedStrings #-}

module CROE.Backend.IntTest.APISpec.User
  ( spec
  ) where

import           Data.Proxy                        (Proxy (..))
import           Data.Text                         (Text)
import           Servant.API
import           Servant.Client
import           Test.Hspec

import           CROE.Backend.IntTest.APISpec.Base (Server (..))
import           CROE.Common.API.User

spec :: SpecWith Server
spec =
    describe "register" $
      it "returns 200" $ \server -> do
        r <- runClientM (register (Just "steven172307@gmail.com")) (_server_clientEnv server)
        r `shouldBe` Right NoContent

register :: Maybe Text -> ClientM NoContent
putProfile :: BasicAuthData -> User -> ClientM Text
putProfile :<|> register = client (Proxy :: Proxy API)
