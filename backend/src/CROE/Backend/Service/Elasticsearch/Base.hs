{-# LANGUAGE QuasiQuotes #-}

module CROE.Backend.Service.Elasticsearch.Base
  ( Config(..)
  , runElasticsearch
  ) where

import           Control.Exception
import           Control.Lens
import           Data.Aeson                               hiding ((.=))
import qualified Data.Aeson                               as Aeson
import           Data.String.Interpolate
import           Data.Text                                (Text)
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
  PutTask docId task -> putTaskImpl config docId task
  UpdateTaskStatus docId taskStatus -> updateTaskStatusImpl config docId taskStatus

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

statusIsSuccess :: Int -> Bool
statusIsSuccess status = status >= 200 && status < 300

instance FromJSON Config where
  parseJSON = genericParseJSON aesonOptions

taskToDoc :: Task -> Value
taskToDoc task = object
    [ "title" Aeson..= (task ^. task_title)
    , "abstract" Aeson..= (task ^. task_abstract)
    , "reward" Aeson..= (task ^. task_reward)
    , "creatorId" Aeson..= (task ^. task_creatorId)
    , "creatorScore" Aeson..= (task ^. task_creatorScore)
    , ("duration", object
        [ "gte" Aeson..= fst (task ^. task_duration)
        , "lte" Aeson..= snd (task ^. task_duration)
        ]
      )
    , "location" Aeson..= (task ^. task_location)
    , "takerId" Aeson..= (task ^. task_takerId)
    , "status" Aeson..= (task ^. task_status)
    ]
