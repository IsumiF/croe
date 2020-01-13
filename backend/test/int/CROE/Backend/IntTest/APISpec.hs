module CROE.Backend.IntTest.APISpec
  ( spec
  ) where

import           Test.Hspec

import           CROE.Backend.IntTest.APISpec.Base
import qualified CROE.Backend.IntTest.APISpec.User as User

spec :: Spec
spec =
    beforeAll setupSpec $ afterAll tearDownSpec $
      User.spec
