{-# LANGUAGE BangPatterns #-}

module CROE.Backend.Service.School
  ( getSchoolList
  ) where

import qualified Data.Map.Strict            as Map
import           Data.Maybe
import           Database.Persist           (Entity (..))
import           Database.Persist.Sql       (fromSqlKey)
import           Polysemy
import           Servant.Server

import qualified CROE.Backend.Persist.Class as Persist
import           CROE.Backend.Persist.Types
import qualified CROE.Common.School         as Common
import qualified CROE.Common.User           as Common

getSchoolList ::
  Members '[ Persist.ConnectionPool
           , Persist.ReadEntity School
           , Persist.ReadEntity SchoolDomain
           , Persist.ReadEntity SchoolCampus
           ] r
  => Common.User
  -> Sem r (Either ServerError [Common.School])
getSchoolList _ =
    Persist.withConn $ \conn -> do
      schools :: [Entity School] <- Persist.selectList conn [] []
      schoolCampuses :: [Entity SchoolCampus] <- Persist.selectList conn [] []
      schoolDomains :: [Entity SchoolDomain] <- Persist.selectList conn [] []
      let !schoolToCampuses = Map.fromListWith mappend
            (fmap (\e -> (schoolCampusSchoolId (entityVal e), [e])) schoolCampuses)
          !schoolToDomains = Map.fromListWith mappend
            (fmap (\e -> (schoolDomainSchoolId (entityVal e), [e])) schoolDomains)
          schoolEntityToCommon entity =
            let name = schoolName (entityVal entity)
                schoolId = entityKey entity
                campusEntities = fromMaybe [] (schoolToCampuses Map.!? schoolId)
                campusList = fmap (\e -> Common.SchoolCampus
                  (fromSqlKey (entityKey e)) (schoolCampusName (entityVal e))
                  ) campusEntities
                domainEntities = fromMaybe [] (schoolToDomains Map.!? schoolId)
                domainList = fmap (schoolDomainDomain . entityVal) domainEntities
             in Common.School
                  { Common._school_name = name
                  , Common._school_campusList = campusList
                  , Common._school_domains = domainList
                  }
      pure (Right (fmap schoolEntityToCommon schools))
