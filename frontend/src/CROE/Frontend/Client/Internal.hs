{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TemplateHaskell       #-}

module CROE.Frontend.Client.Internal where

import           Control.Lens
import           Data.Aeson
import           Data.Proxy                     (Proxy (..))
import           GHC.Generics                   (Generic)
import           Reflex.Dom                     hiding (Client)
import           Servant.API
import           Servant.Reflex                 hiding (Client)

import           CROE.Common.API                (API)
import           CROE.Common.Util               (aesonOptions)
import           CROE.Frontend.Client.Protected
import           CROE.Frontend.Client.User

newtype Config = Config
    { _config_baseUrl :: BaseUrl
    } deriving (Generic, Show, Eq)

instance FromJSON Config where
  parseJSON = genericParseJSON aesonOptions

data Client t m = Client
  { _client_user      :: UserClient t m
  , _client_protected :: Dynamic t (Maybe BasicAuthData) -> ProtectedClient t m
  }

makeLenses ''Client

newClient :: forall t m. MonadWidget t m => Config -> Client t m
newClient (Config baseUrl) =
    let ((userProtected :<|> _applyCode :<|> _register :<|> _validateEmail)
          :<|> protected) =
          client (Proxy :: Proxy API) (Proxy :: Proxy m) (Proxy :: Proxy ())
            (constDyn baseUrl)
        _putProfile user = let (f :<|> _) = userProtected user in f
        _getProfile user = let (_ :<|> g) = userProtected user in g
        _client_user = UserClient{..}

        _client_protected user =
          let ( _taskClient_new
                :<|> _taskClient_update
                :<|> _taskClient_changeStatus
                :<|> _taskClient_get
                :<|> _taskClient_search
                :<|> _taskClient_reindex
                :<|> _taskClient_addReview
                )
                :<|> _schoolClient_get
                :<|> (
                  _chatClient_messages
                  :<|> _chatClient_contactList
                  :<|> _chatClient_totalUnreadCount
                  :<|> _chatClient_markAsRead
                )
                  = protected user
              _protectedClient_task = TaskClient{..}
              _protectedClient_school = SchoolClient{..}
              _protectedClient_chat = ChatClient{..}
           in ProtectedClient{..}
     in Client{..}
