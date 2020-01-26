{-# LANGUAGE RecursiveDo #-}

module CROE.Frontend.Widget
  ( primaryWidget
  ) where

import           Control.Lens
import           Control.Monad.IO.Class
import           Data.Maybe                      (fromMaybe)
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import qualified Data.Text.Encoding              as T
import           Reflex.Dom
import           Reflex.Dom.Bulma.Component.Show (showWidget)
import           Reflex.Dom.Contrib.Router       (route)
import           URI.ByteString

import           CROE.Frontend.Client
import           CROE.Frontend.Env
import           CROE.Frontend.Widget.Entrance   (entranceWidget)

primaryWidget :: (MonadWidget t m)
              => Env t m
              -> m ()
primaryWidget env = mdo
    routeDyn <- route updateRoute
    let isEntrance = fmap (("/r/entrance" `T.isPrefixOf`) . uriPathT) routeDyn
    (userPwd, entranceUpdateRoute) <- showWidget isEntrance $
      entranceWidget (env ^. env_client . client_user) (fmap (stripPrefix' "/r/entrance" . uriPathT) routeDyn)
    let updateRoute = fmap ("/r/entrance" <>) entranceUpdateRoute
    -- debug print
    performEvent_ $ ffor (updated userPwd) (liftIO . print)

uriPathT :: URIRef Absolute -> Text
uriPathT = T.decodeUtf8 . uriPath

stripPrefix' :: Text -> Text -> Text
stripPrefix' prefix str = fromMaybe "" (T.stripPrefix prefix str)
