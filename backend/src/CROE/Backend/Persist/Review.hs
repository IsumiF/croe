{-# LANGUAGE QuasiQuotes #-}

module CROE.Backend.Persist.Review
  ( ReviewRepo(..)
  , averageRating
  , runReviewRepo
  ) where

import           CROE.Backend.Persist.Internal (Connection, RawSqlRunner,
                                                rawSql)
import           CROE.Backend.Persist.Types
import           Data.String.Interpolate       (i)
import           Database.Persist.Sql          (toPersistValue, unSingle)
import           Polysemy

data ReviewRepo (m :: * -> *) a where
  AverageRating :: Connection -> UserId -> ReviewRepo m (Maybe Double)

makeSem ''ReviewRepo

runReviewRepo :: Members '[RawSqlRunner] r
              => Sem (ReviewRepo : r) a
              -> Sem r a
runReviewRepo = interpret $ \case
  AverageRating conn userId -> do
    result <- rawSql conn
      [i|
        SELECT AVG(rating)
        FROM `review`
        WHERE `review`.`user` = ?
      |] [ toPersistValue userId ]
    pure (unSingle . head $ result)
