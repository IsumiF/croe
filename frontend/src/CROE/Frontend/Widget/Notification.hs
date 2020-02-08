{-# LANGUAGE RecursiveDo     #-}
{-# LANGUAGE TemplateHaskell #-}

module CROE.Frontend.Widget.Notification
  ( notification
  , NotificationConfig(..)
  , notificationConfig_show
  ) where

import           Control.Lens
import           Data.Default
import           Data.Functor                      (void)
import           Data.Text                         (Text)
import           Reflex.Dom
import           Reflex.Dom.Bulma.Component.Button (buttonAttr)

newtype NotificationConfig t = NotificationConfig
  { _notificationConfig_show    :: Event t Text
  }

makeLenses ''NotificationConfig

instance Reflex t => Default (NotificationConfig t) where
  def = NotificationConfig
    never

notification :: MonadWidget t m
             => NotificationConfig t
             -> m ()
notification (NotificationConfig showEvt) = mdo
    manualClose <- elDynClass "div" classList $ do
      (closeEvt', _) <- buttonAttr ("class" =: "delete") blank
      message <- holdDyn "未知错误" showEvt
      dynText message
      pure closeEvt'
    autoClose <- delay 3 (void showEvt)
    let closeEvt = leftmost [manualClose, autoClose]

    let hidden = not <$> leftmost [fmap (const False) closeEvt, fmap (const True) showEvt]
        hiddenClass = (\p -> if p then " is-hidden" else "") <$> hidden
    hiddenClassDyn <- holdDyn " is-hidden" hiddenClass
    let classList = ("notification is-danger is-light" <>) <$> hiddenClassDyn
    blank
