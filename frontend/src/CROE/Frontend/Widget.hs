module CROE.Frontend.Widget
  ( primaryWidget
  ) where

import           Reflex.Dom

primaryWidget :: MonadWidget t m
              => m ()
primaryWidget = blank
