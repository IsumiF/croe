{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StrictData  #-}

module CROE.Backend.Service.Elasticsearch.Base
  ( Config(..)
  , runElasticsearch
  ) where

import           Control.Exception
import           Control.Lens                             hiding ((.=))
import           Data.Aeson
import qualified Data.Aeson                               as Aeson
import           Data.Aeson.Types                         (Pair)
import           Data.Coerce
import           Data.Int
import           Data.Proxy                               (Proxy)
import           Data.String.Interpolate
import           Data.Text                                (Text)
import qualified Data.Text                                as T
import qualified Data.Text.Encoding                       as T
import           GHC.Generics                             (Generic)
import           Network.HTTP.Req
import           Polysemy

import           CROE.Backend.Logger.Class
import           CROE.Backend.Service.Elasticsearch.Class
import           CROE.Common.Task
import           CROE.Common.Util                         (aesonOptions)

data Config = Config
  { _config_host        :: Text
  , _config_port        :: Int
  , _config_username    :: Text
  , _config_password    :: Text
  , _config_indexPrefix :: Text
  } deriving (Show, Eq, Generic)

runElasticsearch :: Members '[Embed IO, Logger] r
                 => Config
                 -> Sem (Elasticsearch : r) a
                 -> Sem r a
runElasticsearch config = interpret $ \case
  SearchTask condition -> searchTaskImpl config condition
  PutTask docId task -> putTaskImpl config docId task
  UpdateTaskStatus docId taskStatus -> updateTaskStatusImpl config docId taskStatus
  UpdateCreatorName creatorId creatorName ->
    updateByQuery config creatorId "creatorName" (toJSON creatorName)
  UpdateCreatorScore creatorId creatorScore ->
    updateByQuery config creatorId "creatorScore" (toJSON creatorScore)

reqOptions :: Config -> Option 'Http
reqOptions config = port port' <> basicAuthUnsafe (T.encodeUtf8 username) (T.encodeUtf8 password)
  where
    port' = _config_port config
    username = _config_username config
    password = _config_password config

putTaskImpl :: Members '[Embed IO, Logger] r
            => Config
            -> Text
            -> Task
            -> Sem r Bool
putTaskImpl config docId task = do
    respEither <- embed $ try $ runReq defaultHttpConfig $
      req PUT
        (http host /: (indexPrefix <> "task") /: "_doc" /~ docId)
        (ReqBodyJson (taskToDoc task))
        ignoreResponse
        (reqOptions config)
    case respEither of
      Left (e :: HttpException) -> do
        printLogt LevelError [i|fail to save task to elasticsearch, task: #{task}, id: #{docId}, excpetion: #{e}|]
        pure False
      Right resp -> do
        let status = responseStatusCode resp
        if not (statusIsSuccess status)
        then do
          printLogt LevelError [i|fail to save task to elasticsearch, task: #{task}, id: #{docId}, status: #{status}|]
          pure False
        else pure True
  where
    host = _config_host config
    indexPrefix = _config_indexPrefix config

updateTaskStatusImpl :: Members '[Embed IO, Logger] r
                     => Config
                     -> Text
                     -> TaskStatus
                     -> Sem r Bool
updateTaskStatusImpl config@(Config host _ _ _ indexPrefix) taskId status = do
    respEither <- embed $ try $ runReq defaultHttpConfig $
      req POST
        (http host /: (indexPrefix <> "task") /: "_update" /~ taskId)
        (ReqBodyJson updateStatusReqBody)
        ignoreResponse
        (reqOptions config)
    case respEither of
      Left (e :: HttpException) -> do
        printLogt LevelError [i|fail to update status of task, id: #{taskId}, status: #{status}, exception: #{e}|]
        pure False
      Right resp -> do
        let httpStatus = responseStatusCode resp
        if statusIsSuccess httpStatus
        then pure True
        else do
          printLogt LevelError [i|fail to update status of task, id: #{taskId}, status: #{status}, http status: #{httpStatus}|]
          pure False
  where
    updateStatusReqBody = object
      [ "doc" Aeson..= object [ "status" Aeson..= status ]
      ]

searchTaskImpl :: Members '[Embed IO, Logger] r
               => Config
               -> TaskQueryCondition
               -> Sem r TaskSearchResult
searchTaskImpl config@(Config host _ _ _ indexPrefix) queryCondition = do
    respEither <- embed $ try $ runReq defaultHttpConfig $
      req POST
        (http host /: (indexPrefix <> "task") /: "_search")
        (ReqBodyJson (queryConditionToEsJson queryCondition))
        (jsonResponse :: Proxy (JsonResponse (SearchResponse TaskDoc)))
        (reqOptions config)
    case respEither of
      Left (e :: HttpException) -> do
        printLogt LevelError [i|es search failed with exception #{e}|]
        pure emptyTaskSearchResult
      Right resp -> do
        let httpStatus = responseStatusCode resp
        if not (statusIsSuccess httpStatus)
        then do
          printLogt LevelError [i|es search failed with http status #{httpStatus}|]
          pure emptyTaskSearchResult
        else do
          let body = responseBody resp
              hits = _searchResponse_hits body
              total = _hits_total hits
              taskHits = _hits_values hits
              tasks = fmap (\(Hit tId task) -> (tId, coerce task)) taskHits
          pure $ TaskSearchResult total tasks

statusIsSuccess :: Int -> Bool
statusIsSuccess status = status >= 200 && status < 300

instance FromJSON Config where
  parseJSON = genericParseJSON aesonOptions

newtype SearchResponse doc = SearchResponse
  { _searchResponse_hits :: Hits doc
  } deriving (Show, Eq, Generic)

instance FromJSON doc => FromJSON (SearchResponse doc) where
  parseJSON = genericParseJSON aesonOptions

data Hits doc = Hits
  { _hits_total  :: Integer
  , _hits_values :: [Hit doc]
  } deriving (Show, Eq)

instance FromJSON doc => FromJSON (Hits doc) where
  parseJSON = withObject "Hits" $ \o -> do
    total <- o .: "total"
    _hits_total <- withObject "Hits.total" (.: "value") total
    _hits_values <- o .: "hits"
    pure Hits{..}

data Hit doc = Hit
  { _hit__id     :: Text
  , _hit__source :: doc
  } deriving (Show, Eq, Generic)

instance FromJSON doc => FromJSON (Hit doc) where
  parseJSON = genericParseJSON aesonOptions

newtype TaskDoc = TaskDoc Task

instance ToJSON TaskDoc where
  toJSON (TaskDoc task) = taskToDoc task

taskToDoc :: Task -> Value
taskToDoc task = object
    [ "title" Aeson..= (task ^. task_title)
    , "abstract" Aeson..= (task ^. task_abstract)
    , "reward" Aeson..= (task ^. task_reward)
    , "creatorId" Aeson..= (task ^. task_creatorId)
    , "creatorName" Aeson..= (task ^. task_creatorName)
    , "creatorScore" Aeson..= (task ^. task_creatorScore)
    , ("duration", object
        [ "gte" Aeson..= fst (task ^. task_duration)
        , "lte" Aeson..= snd (task ^. task_duration)
        ]
      )
    , "location" Aeson..= (task ^. task_location)
    , "campusId" Aeson..= (task ^. task_campusId)
    , "takerId" Aeson..= (task ^. task_takerId)
    , "status" Aeson..= (task ^. task_status)
    , "reviewedByUsers" Aeson..= (task ^. task_reviewedByUsers)
    ]

data Range a = Range
  { _range_lte :: a
  , _range_gte :: a
  } deriving (Show, Eq, Generic)

instance FromJSON a => FromJSON (Range a) where
  parseJSON = genericParseJSON aesonOptions

instance FromJSON TaskDoc where
  parseJSON = withObject "TaskDoc" $ \o -> do
    _task_title <- o .: "title"
    _task_abstract <- o .: "abstract"
    _task_reward <- o .: "reward"
    _task_creatorId <- o .: "creatorId"
    _task_creatorName <- o .: "creatorName"
    _task_creatorScore <- o .: "creatorScore"
    Range lte gte <- o .: "duration"
    let _task_duration = (gte, lte)
    _task_location <- o .: "location"
    _task_campusId <- o .: "campusId"
    _task_takerId <- o .: "takerId"
    _task_status <- o .: "status"
    _task_reviewedByUsers <- o .: "reviewedByUsers"
    pure . TaskDoc $ Task{..}

queryConditionToEsJson :: TaskQueryCondition -> Value
queryConditionToEsJson queryCondition = object
    [ "query" Aeson..= object
        [ "bool" Aeson..= object
            [ "must" Aeson..= object
                [ strQueryToEsJson (queryCondition ^. taskQueryCondition_query) ]
            , "filter" Aeson..= filters
            ]
        ]
    , "from" Aeson..= (queryCondition ^. taskQueryCondition_offset)
    , "size" Aeson..= (queryCondition ^. taskQueryCondition_limit)
    ]
  where
    filters = termQuery (queryConditionTerms queryCondition)
      <> queryConditionRanges queryCondition

strQueryToEsJson :: Text -> Pair
strQueryToEsJson strQuery
    | T.null strQuery = "match_all" Aeson..= object []
    | otherwise =
        "multi_match" Aeson..= object
          [ "query" Aeson..= strQuery
          , "fields" Aeson..= (["title^3", "abstract"] :: [Text])
          , "type" Aeson..= ("most_fields" :: Text)
          ]

queryConditionTerms :: TaskQueryCondition -> [(Text, Value)]
queryConditionTerms TaskQueryCondition{..} = filterJustSnd
    [ ("reward", fmap toJSON _taskQueryCondition_rewardRange)
    , ("campusId", fmap toJSON _taskQueryCondition_campusId)
    , ("status", fmap toJSON _taskQueryCondition_status)
    , ("creatorId", fmap toJSON _taskQueryCondition_creatorId)
    , ("takerId", fmap toJSON _taskQueryCondition_takerId)
    ]

filterJustSnd :: [(a, Maybe b)] -> [(a, b)]
filterJustSnd [] = []
filterJustSnd ((x, y) : zs) = case y of
    Nothing -> filterJustSnd zs
    Just y' -> (x, y') : filterJustSnd zs

termQuery :: [(Text, Value)] -> [Value]
termQuery = fmap filterToEsJson
  where
    filterToEsJson (fieldName, value) = object
      [ "term" Aeson..= object [ fieldName Aeson..= value ]
      ]

queryConditionRanges :: TaskQueryCondition -> [Value]
queryConditionRanges queryCondition =
    case rewardRange of
      Nothing -> []
      Just (lower, higher) -> pure $ object
        [ "range" Aeson..= object
            [ "reward" Aeson..= object
                [ "gte" Aeson..= lower
                , "lte" Aeson..= higher
                ]
            ]
        ]
  where
    rewardRange = queryCondition ^. taskQueryCondition_rewardRange

updateByQuery :: Members '[Embed IO, Logger] r
              => Config
              -> Int64 -- ^creator id
              -> Text -- ^field name
              -> Value -- ^field new value
              -> Sem r Bool
updateByQuery config@(Config host _ _ _ indexPrefix) creatorId fieldName fieldNewValue = do
    respEither <- embed $ try $ runReq defaultHttpConfig $
      req POST
        (http host /: (indexPrefix <> "task") /: "_update_by_query")
        (ReqBodyJson body)
        ignoreResponse
        (reqOptions config <> "conflicts" =: ("proceed" :: Text))
    case respEither of
      Left (e :: HttpException) -> do
        printLogt LevelError [i|es update by query failed with exception #{e}|]
        pure False
      Right resp -> do
        let httpStatus = responseStatusCode resp
        if not (statusIsSuccess httpStatus)
        then do
          printLogt LevelError [i|es search failed with http status #{httpStatus}|]
          pure False
        else pure True
  where
    body = object
      [ "script" .= object
          [ "lang" .= ("painless" :: Text)
          , "source" .= ([i|ctx._source.#{fieldName} = params.#{fieldName}|] :: Text)
          , "params" .= object
              [ fieldName .= fieldNewValue
              ]
          ]
      , "query" .= object
          [ "term" .= object
              [ "creatorId" .= creatorId
              ]
          ]
      ]
