{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module CROE.Backend.Web
  ( mkWebApp
  ) where

import           Control.Monad.Except
import           Data.Proxy                  (Proxy (..))
import           Network.HTTP.Types
import           Network.Wai.Middleware.Cors
import           Servant

import           CROE.Backend.Env
import qualified CROE.Backend.Service        as Service
import           CROE.Common.API

handler :: ServerT APIWithStatic (ExceptT ServantErr App)
handler = apiHandler :<|> serveDirectoryWebApp "static/"

apiHandler :: ServerT API (ExceptT ServantErr App)
apiHandler = Service.login

toRawHandler :: Env -> ExceptT ServantErr App a -> Handler a
toRawHandler env appE = Handler $ do
    let app = runExceptT appE
    ExceptT $ runApp env app

standardHandler :: Env -> Server APIWithStatic
standardHandler env = hoistServer apiProxy (toRawHandler env) handler

mkWebApp :: Env -> Application
mkWebApp env = cors (const (Just corsPolicy)) app
  where
    app = serve apiProxy (standardHandler env)

type APIWithStatic = API :<|> Raw

apiProxy :: Proxy APIWithStatic
apiProxy = Proxy

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
