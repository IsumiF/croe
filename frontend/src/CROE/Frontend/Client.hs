module CROE.Frontend.Client
  ( Client
  , client_user
  , client_protected
  , reqResultToEither
  , messageOnReqError
  , module Exports
  ) where

import           Data.Maybe                     (fromMaybe)
import           Data.Text                      (Text)
import           Reflex.Dom                     hiding (Client)
import           Servant.Reflex                 hiding (Client)

import           CROE.Frontend.Client.Init
import           CROE.Frontend.Client.Protected as Exports
import           CROE.Frontend.Client.User      as Exports

reqResultToEither :: ReqResult tag a -> Either Text a
reqResultToEither (ResponseSuccess _ a _) = Right a
reqResultToEither (ResponseFailure _ msg xhrResponse) =
    Left $ fromMaybe msg (_xhrResponse_responseText xhrResponse)
reqResultToEither (RequestFailure _ msg ) = Left msg

messageOnReqError :: Reflex t => Text -> Event t (ReqResult tag a) -> Event t Text
messageOnReqError msg =
    fmap (const msg) . filterLeft . fmap reqResultToEither
