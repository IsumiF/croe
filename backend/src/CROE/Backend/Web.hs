{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module CROE.Backend.Web
  ( mkWebApp
  ) where

import           Control.Monad.Except
import           Data.Proxy                      (Proxy (..))
import           Network.HTTP.Types
import           Network.Wai.Middleware.Cors
import           Polysemy
import           Servant

import           CROE.Backend.Env
import           CROE.Backend.Service.Auth.Class (checkBasicAuth)
import           CROE.Backend.Web.ApiHandler
import           CROE.Backend.Web.WebSocket      (wsHandler)
import           CROE.Common.API
import           Servant.API.WebSocket

type APIWithStatic = API :<|> APIWebSocket :<|> Raw

type APIWebSocket = "ws" :> BasicAuth "croe" User :> WebSocket

handler :: ServerT APIWithStatic (ExceptT ServerError (Sem AppEffects))
handler = apiHandler :<|> wsHandler :<|> serveDirectoryWebApp "static/"

toRawHandler :: Env -> ExceptT ServerError (Sem AppEffects) a -> Handler a
toRawHandler env app = do
    resultEither <- liftIO $ runApp env (runExceptT app)
    liftEither resultEither

standardHandler :: Env -> Server APIWithStatic
standardHandler env =
    hoistServerWithContext
      apiProxy
      (Proxy :: Proxy '[BasicAuthCheck User])
      (toRawHandler env)
      handler

mkWebApp :: Env -> Application
mkWebApp env = cors (const (Just corsPolicy)) app
  where
    app = serveWithContext apiProxy (context env) (standardHandler env)

apiProxy :: Proxy APIWithStatic
apiProxy = Proxy

context :: Env -> Context (BasicAuthCheck User ': '[])
context env = BasicAuthCheck (runApp env . checkBasicAuth) :. EmptyContext

corsPolicy :: CorsResourcePolicy
corsPolicy =
  let requestHeaders = "content-type" : "Authorization" : corsRequestHeaders simpleCorsResourcePolicy
   in simpleCorsResourcePolicy
        { corsRequestHeaders = requestHeaders
        , corsMethods =
           [ methodGet
           , methodPut
           , methodDelete
           , methodPost
           , methodPatch
           , methodHead
           ]
        }
