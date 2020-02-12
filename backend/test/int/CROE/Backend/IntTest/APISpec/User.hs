{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module CROE.Backend.IntTest.APISpec.User
  ( spec
  ) where

import           Control.Lens
import           Servant.API
import           Test.Hspec

import           CROE.Backend.IntTest.APISpec.Base
import           CROE.Common.API.User

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
  where
    applyCode = servantClient ^. (client_user . userClient_applyCode)
    validateEmail = servantClient ^. (client_user . userClient_validateEmail)
