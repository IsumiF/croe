module CROE.Common.Util
  ( aesonOptions
  , utf8LBS
  ) where

import           Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import           Data.Text            (Text)
import qualified Data.Text.Encoding   as T

aesonOptions :: Options
aesonOptions = defaultOptions
  { fieldLabelModifier = tail . dropWhile (/= '_') . tail
  }

utf8LBS :: Text -> LBS.ByteString
utf8LBS = LBS.fromStrict . T.encodeUtf8
