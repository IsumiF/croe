module CROE.Common.Util
  ( aesonOptions
  ) where

import           Data.Aeson

aesonOptions :: Options
aesonOptions = defaultOptions
  { fieldLabelModifier = tail . dropWhile (/= '_') . tail
  }
