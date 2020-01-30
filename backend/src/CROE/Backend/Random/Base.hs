module CROE.Backend.Random.Base
  ( runRandomService
  , module CROE.Backend.Random.Class
  ) where

import qualified Control.Monad.Random.Class as MonadRandom
import           Polysemy

import           CROE.Backend.Random.Class

runRandomService :: Member (Embed IO) r
                 => Sem (RandomService : r) a
                 -> Sem r a
runRandomService = interpret $ \case
    GetRandomR range -> embed $ MonadRandom.getRandomR range
