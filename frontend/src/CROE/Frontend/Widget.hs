{-# LANGUAGE LambdaCase  #-}
{-# LANGUAGE RecursiveDo #-}

module CROE.Frontend.Widget
  ( primaryWidget
  ) where

import           Control.Lens
import           Data.Maybe
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import qualified Data.Text.Encoding              as T
import           Reflex.Dom
import           Reflex.Dom.Bulma.Component.Show (showWidget)
import           Reflex.Dom.Contrib.Router       (route)
import           URI.ByteString

import           CROE.Frontend.Client
import           CROE.Frontend.Env
import           CROE.Frontend.User
import           CROE.Frontend.Widget.Entrance   (entranceWidget)
import           CROE.Frontend.Widget.TaskList   (taskListWidget)

primaryWidget :: MonadWidget t m
              => Env t m
              -> m ()
primaryWidget env = mdo
    routeDyn <- route updateRoute
    let routeIsEntrance = fmap ((entranceRoute `T.isPrefixOf`) . uriPathT) routeDyn
        isEntrance = (||) <$> routeIsEntrance <*> fmap not hasUser
    (userPwd, entranceUpdateRoute) <- showWidget isEntrance $
      entranceWidget (env ^. env_client . client_user) (fmap (stripPrefix' entranceRoute . uriPathT) routeDyn)
    let updateRoute = leftmost
          [ fmap (entranceRoute <>) entranceUpdateRoute
          , fmap (const taskListRoute) (ffilter id $ updated hasUser)
          ]
        hasUser = fmap isJust userPwd
        authDataDyn = (fmap . fmap) toBasicAuthData userPwd
        protectedClient = (env ^. env_client . client_protected) authDataDyn

    dyn_ $ ffor userPwd $ \case
      Nothing -> blank
      Just userPwd' ->
        taskListWidget (userPwd' ^. userPassword_user) protectedClient

    blank
  where
    entranceRoute = "/r/entrance"
    taskListRoute = "/r/task_list"

uriPathT :: URIRef Absolute -> Text
uriPathT = T.decodeUtf8 . uriPath

stripPrefix' :: Text -> Text -> Text
stripPrefix' prefix str = fromMaybe "" (T.stripPrefix prefix str)
