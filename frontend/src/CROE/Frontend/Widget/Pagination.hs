{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData          #-}
{-# LANGUAGE TemplateHaskell     #-}

module CROE.Frontend.Widget.Pagination
  ( pagination
  , PaginationConfig
  , paginationConfig_total
  , paginationConfig_size
  , Pagination
  , pagination_current
  ) where

import           Control.Lens
import           CROE.Common.Util                  (showt)
import           Data.Default
import           Data.Map.Strict                   (Map)
import           Data.Text                         (Text)
import           Reflex.Dom                        hiding (current)
import           Reflex.Dom.Bulma.Component.Button (linkButtonDynAttr_)
import           Reflex.Dom.Contrib.CssClass

data PaginationConfig t = PaginationConfig
  { _paginationConfig_total :: Dynamic t Integer
  , _paginationConfig_size  :: PaginationSize
  }

data PaginationSize = PgSizeIsDefault | PgSizeIsSmall | PgSizeIsMedium | PgSizeIsLarge

pgSizeToClass :: PaginationSize -> CssClass
pgSizeToClass PgSizeIsDefault = mempty
pgSizeToClass PgSizeIsSmall   = "is-small"
pgSizeToClass PgSizeIsMedium  = "is-medium"
pgSizeToClass PgSizeIsLarge   = "is-large"

instance Reflex t => Default (PaginationConfig t) where
  def = PaginationConfig
    { _paginationConfig_total = constDyn 1
    , _paginationConfig_size = PgSizeIsDefault
    }

makeLenses ''PaginationConfig

newtype Pagination t = Pagination
  { _pagination_current :: Dynamic t Integer
  }

makeLenses ''Pagination

pagination :: forall t m. MonadWidget t m
           => PaginationConfig t
           -> m (Pagination t)
pagination (PaginationConfig total size) =
    elAttr "nav" ("class" =: renderClass ("pagination" <> pgSizeToClass size) <> "role" =: "navigation") $
      elClass "ul" "pagination-list" $ mdo
        let current :: Dynamic t Integer
            firstItemClass = fmap (\c -> itemClass False (c >= 2)) current
            leftEllipsisClass = fmap (\c -> ellipsisClass (c >= 4)) current
            prevItemClass = fmap (\c -> itemClass False (c >= 3)) current
            currentItemClass = constDyn (itemClass True True)
            nextItemClass = (\c t -> itemClass False (c <= t - 2)) <$> current <*> total
            rightEllipsisClass = (\c t -> ellipsisClass (c <= t - 3)) <$> current <*> total
            lastItemClass = (\c t -> itemClass False (c <= t - 1)) <$> current <*> total

        firstClick <- el "li" $ linkButtonDynAttr_ (renderClassDynAttr firstItemClass) $ text "1"
        el "li" $ elDynClass "span" (renderClass <$> leftEllipsisClass) $ text hellip
        prevClick <- el "li" $ linkButtonDynAttr_ (renderClassDynAttr prevItemClass) $ dynText (fmap (showt . (+ (-1))) current)
        _ <- el "li" $ linkButtonDynAttr_ (renderClassDynAttr currentItemClass) $ dynText (fmap showt current)
        nextClick <- el "li" $ linkButtonDynAttr_ (renderClassDynAttr nextItemClass) $ dynText (fmap (showt . (+1)) current)
        el "li" $ elDynClass "span" (renderClass <$> rightEllipsisClass) $ text hellip
        lastClick <- el "li" $ linkButtonDynAttr_ (renderClassDynAttr lastItemClass) $ dynText (fmap showt total)
        let switchPage :: Event t (Integer -> Integer) = leftmost
              [ fmap (\_ _ -> 1) firstClick
              , fmap (const (+ (-1))) prevClick
              , fmap (const (+1)) nextClick
              , fmap const (tagPromptlyDyn total lastClick)
              ]
        current <- foldDyn ($) 1 switchPage
        pure $ Pagination current
  where
    itemClass :: Bool -- ^is current
              -> Bool -- ^visible
              -> CssClass
    itemClass isCurrent visible = "pagination-link"
      <> (if isCurrent then "is-current" else mempty)
      <> (if visible then mempty else "is-hidden")
    ellipsisClass :: Bool -- ^visible
                  -> CssClass
    ellipsisClass visible = "pagination-ellipsis"
      <> (if visible then mempty else "is-hidden")
    renderClassDynAttr :: Dynamic t CssClass -> Dynamic t (Map Text Text)
    renderClassDynAttr = fmap (\c -> "class" =:  renderClass c)

hellip :: Text
hellip = "…"
