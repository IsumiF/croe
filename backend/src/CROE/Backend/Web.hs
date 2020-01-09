{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module CROE.Backend.Web
  ( mkWebApp
  ) where

import           Control.Monad.Except
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Reader
import           Data.Proxy                  (Proxy (..))
import qualified Data.Text.IO                as T
import           Network.HTTP.Types
import           Network.Wai.Middleware.Cors
import           Servant

import           CROE.Backend.Env
import qualified CROE.Backend.Service.Auth   as Service
import           CROE.Common.API

type APIWithStatic = API :<|> Raw

handler :: ServerT APIWithStatic (ExceptT ServerError App)
handler = apiHandler :<|> serveDirectoryWebApp "static/"

apiHandler :: ServerT API (ExceptT ServerError App)
apiHandler = (\_ _ -> pure "hello") :<|> (\(Just email) -> liftIO (T.putStrLn email) >> pure NoContent)

toRawHandler :: Env -> ExceptT ServerError App a -> Handler a
toRawHandler env appE = Handler $ do
    let app = runExceptT appE
    ExceptT $ runApp env app

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
context env = BasicAuthCheck (\x -> runReaderT (Service.checkBasicAuth x) env) :. EmptyContext

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
