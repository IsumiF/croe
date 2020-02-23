module CROE.Common.Util
  ( aesonOptions
  , utf8LBS
  , reverseMap
  , showt
  , readt
  , readtMaybe
  , safeHead
  , liftBool
  ) where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Data.Aeson
import qualified Data.ByteString.Lazy      as LBS
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as Map
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as T
import           Text.Read                 (readMaybe)

aesonOptions :: Options
aesonOptions = defaultOptions
  { fieldLabelModifier = tail . dropWhile (/= '_') . tail
  }

utf8LBS :: Text -> LBS.ByteString
utf8LBS = LBS.fromStrict . T.encodeUtf8

reverseMap :: (Ord a, Ord b) => Map a b -> Map b a
reverseMap = Map.fromList . fmap (\(a, b) -> (b, a)) . Map.toList

showt :: Show a => a -> Text
showt = T.pack . show

readt :: Read a => Text -> a
readt = read . T.unpack

readtMaybe :: Read a => Text -> Maybe a
readtMaybe = readMaybe . T.unpack

safeHead :: [a] -> Maybe a
safeHead (x:_) = Just x
safeHead _     = Nothing

liftBool :: Monad m => m Bool -> MaybeT m ()
liftBool m = do
    ok <- lift m
    if ok
    then pure ()
    else MaybeT (pure Nothing)
