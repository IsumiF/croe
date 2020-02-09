module CROE.Common.Util
  ( aesonOptions
  , utf8LBS
  , reverseMap
  ) where

import           Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as Map
import           Data.Text            (Text)
import qualified Data.Text.Encoding   as T

aesonOptions :: Options
aesonOptions = defaultOptions
  { fieldLabelModifier = tail . dropWhile (/= '_') . tail
  }

utf8LBS :: Text -> LBS.ByteString
utf8LBS = LBS.fromStrict . T.encodeUtf8

reverseMap :: (Ord a, Ord b) => Map a b -> Map b a
reverseMap = Map.fromList . fmap (\(a, b) -> (b, a)) . Map.toList
