{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CROE.Backend.Persist.Types.ChatMessageStatus
  ( ChatMessageStatus(..)
  ) where

import qualified CROE.Common.Chat    as Common
import           Data.Aeson
import           Database.Persist.TH

newtype ChatMessageStatus = ChatMessageStatus Common.ChatMessageStatus
  deriving newtype (Show, Read, Eq, Ord, Enum, FromJSON, ToJSON)

derivePersistField "ChatMessageStatus"
