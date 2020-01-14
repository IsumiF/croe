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
      it "apply verification code" $ \server -> do
        r <- runClientM (applyCode (Just "steven172307@gmail.com")) (_server_clientEnv server)
        r `shouldBe` Right NoContent

putProfile :: BasicAuthData -> User -> ClientM NoContent
applyCode :: Maybe Text -> ClientM NoContent
register :: RegisterForm -> ClientM NoContent
putProfile :<|> applyCode :<|> register = client (Proxy :: Proxy API)
