module FrontendUtils.Component.Button
  ( buttonAttr
  ) where

import           Data.Map.Strict (Map)
import           Data.Text       (Text)
import           Reflex.Dom

buttonAttr :: MonadWidget t m => Map Text Text -> m a -> m (Event t (), a)
buttonAttr attr child = do
    (e, a) <- elAttr' "button" attr child
    pure (domEvent Click e, a)
