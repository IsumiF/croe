module CROE.Backend.Random.Class
  ( RandomService(..)
  , getRandomR
  ) where

import           Polysemy
import           System.Random

data RandomService (m :: * -> *) r where
  GetRandomR :: Random a => (a, a) -> RandomService m a

makeSem ''RandomService
