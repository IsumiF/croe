{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module CROE.Backend.Persist.Types.Role
  ( Role(..)
  ) where

import           Database.Persist.TH
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data Role = RoleAdmin | RoleUser
            deriving (Eq, Ord)

derivePersistField "Role"

instance Show Role where
  show = (valueToStr Map.!)

instance Read Role where
  readsPrec _ str = maybe [] (\r -> [(r, "")]) (strToValue Map.!? str)

valueToStr :: Map Role String
valueToStr = Map.fromList [(RoleAdmin, "Admin"), (RoleUser, "User")]

strToValue :: Map String Role
strToValue = Map.fromList . fmap (\(a, b) -> (b, a)) . Map.toList $ valueToStr
