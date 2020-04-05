module CROE.Frontend.Widget.Navbar
  ( navbarWidget
  ) where

import           Reflex.Dom
import           Reflex.Dom.Bulma.Component.Button

navbarWidget :: MonadWidget t m
             => m ()
navbarWidget =
    elAttr "nav" ("class" =: "navbar has-shadow has-background-primary" <> "role" =: "navigation") $ do
      divClass "navbar-brand" $
        elAttr "a" ("class" =: "navbar-item") $
          elAttr "img" ("src" =: "https://bulma.io/images/bulma-logo.png" <> "width" =: "112" <> "height" =: "28") blank
      divClass "navbar-menu" $
        divClass "navbar-end" $
          divClass "navbar-item" $
            divClass "dropdown is-right is-hoverable" $ do
              divClass "dropdown-trigger" $
                elClass "span" "icon is-large" $
                  elClass "i" "fas fa-2x fa-user-circle" blank
              divClass "dropdown-menu" $
                divClass "dropdown-content" $ do
                  (myProfileEvt, _) <- linkButton ("class" =: "dropdown-item") $ text "修改资料"
                  divClass "dropdown-divider" blank
                  (logoutEvt, _) <- linkButton ("class" =: "dropdown-item") $ text "退出登陆"
                  pure ()
