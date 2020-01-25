module CROE.Frontend.Client
  ( Client
  , client_user
  , reqResultToEither
  , module CROE.Frontend.Client.User
  ) where

import           Data.Maybe                (fromMaybe)
import           Data.Text                 (Text)
import           Reflex.Dom                hiding (Client)
import           Servant.Reflex            hiding (Client)

import           CROE.Frontend.Client.Init
import           CROE.Frontend.Client.User

reqResultToEither :: ReqResult tag a -> Either Text a
reqResultToEither (ResponseSuccess _ a _) = Right a
reqResultToEither (ResponseFailure _ msg xhrResponse) =
    Left $ fromMaybe msg (_xhrResponse_responseText xhrResponse)
reqResultToEither (RequestFailure _ msg ) = Left msg
