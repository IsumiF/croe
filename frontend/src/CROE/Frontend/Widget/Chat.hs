module CROE.Frontend.Widget.Chat
  ( chatWidget
  ) where

import           CROE.Frontend.Widget.Pagination
import           Reflex.Dom
import           Reflex.Dom.Bulma.Component.Button

chatWidget :: MonadWidget t m
           => m ()
chatWidget =
    divClass "chatbox-wrapper" $ do
      divClass "chatbox card" $ blank
      let buttonClass = "button is-info is-pulled-right has-badge-rounded has-badge-danger has-badge-large"
      -- buttonDynAttr

      pure ()
