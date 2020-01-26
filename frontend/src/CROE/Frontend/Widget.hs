{-# LANGUAGE RecursiveDo #-}

module CROE.Frontend.Widget
  ( primaryWidget
  ) where

import           Control.Lens
import           Control.Monad.IO.Class
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
    let isEntrance = fmap ((== "entrance") . uriPath) routeDyn
    (userPwd, entranceUpdateRoute) <- showWidget isEntrance $
      entranceWidget (env ^. env_client . client_user) undefined
    let updateRoute = fmap ("entrance" <>) entranceUpdateRoute
    -- debug print
    performEvent_ $ ffor (updated userPwd) (liftIO . print)
