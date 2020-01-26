{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module CROE.Backend.IntTest.APISpec.User
  ( spec
  ) where

import           Control.Lens
import           Data.Proxy                        (Proxy (..))
import           Data.Text                         (Text)
import           Network.HTTP.Types.Status         (Status (..))
import           Servant.API
import           Servant.Client
import           Test.Hspec

import           CROE.Backend.IntTest.APISpec.Base
import           CROE.Common.API.User
import           CROE.Common.User

spec :: SpecWith Server
spec = do
    describe "register" $
      it "apply verification code" $ \server -> do
        r <- runClientM' server $ applyCode (Just "steven172307@gmail.com")
        r `shouldBe` Right NoContent
    describe "validate email" $ do
      it "accepts sysu email" $ \server -> do
        r <- runClientM' server $ validateEmail (EmailAddress "fengzlin" "mail2.sysu.edu.cn")
        r `shouldBe` Right "中山大学"
      it "rejects unknown school" $ \server -> do
        r <- runClientM' server $ validateEmail (EmailAddress "steven" "outlook.com")
        getStatusCode r `shouldBe` Just 404

applyCode :: Maybe Text -> ClientM NoContent
register :: RegisterForm -> ClientM NoContent
validateEmail :: EmailAddress -> ClientM Text
authProtected :<|> applyCode :<|> register :<|> validateEmail = client (Proxy :: Proxy API)

putProfile user = let (f :<|> _) = authProtected user in f
getProfile user = let (_ :<|> g) = authProtected user in g

runClientM' :: Server
            -> ClientM a
            -> IO (Either ClientError a)
runClientM' server m =
    runClientM m (_server_clientEnv server)

getStatusCode :: Either ClientError a -> Maybe Int
getStatusCode =
    fmap (statusCode . responseStatusCode)
      . preview (_Left . _FailureResponse . _2)
