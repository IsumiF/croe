{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module CROE.Backend.Service.User
  ( putProfile
  , getProfile
  , applyCode
  , register
  , validateEmail
  -- *Inernal
  ) where

import           Control.Monad.Except
import           Control.Monad.Logger
import           Control.Monad.Random.Class
import           Control.Monad.Reader
import           Control.Monad.Trans.Maybe
import qualified Crypto.KDF.BCrypt          as BCrypt
import qualified Crypto.Random.Types        as Crypto
import           Data.Function              ((&))
import           Data.Maybe
import           Data.Proxy                 (Proxy)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import           Database.Persist           (Entity (..), (=.), (==.))
import           Servant

import           CROE.Backend.Mail.Class
import           CROE.Backend.Persist.Class
import           CROE.Backend.Time.Class
import qualified CROE.Common.API.User       as Common
import qualified CROE.Common.User           as Common
import           CROE.Common.Util           (utf8LBS)

putProfile :: ( MonadError ServerError m
              , MonadPersist backend m
              , WriteEntity User (ReaderT backend m)
              , ReadEntity User (ReaderT backend m)
              )
           => Proxy backend
           -> Common.User
           -> Common.User
           -> m NoContent
putProfile p authUser newUser = do
    withConn p $
      updateWhere
        [UserEmail ==. oldEmail]
        [ UserEmail =. Common._user_email newUser
        , UserName =. Common._user_name newUser
        ]
    pure NoContent
  where
    oldEmail = Common._user_email authUser

getProfile :: Monad m
           => Proxy backend
           -> Common.User
           -> m Common.User
getProfile _ = pure

applyCode :: ( MonadError ServerError m
             , MonadPersist backend m
             , WriteEntity User (ReaderT backend m)
             , WriteEntity UserRegistry (ReaderT backend m)
             , MonadLogger m
             , MonadMail m
             , MonadRandom m
             , MonadTime m
             )
          => Proxy backend
          -> Maybe Text
          -> m NoContent
applyCode p email =
    case email of
      Just email' -> do
        $(logInfo) $ email' <> " is registering"
        code <- generateCode p email'
        success <- sendPlainTextMail email' "CROE" "校园代办：注册验证码" (verificationCodeMailBody code)
        if success
        then pure NoContent
        else throwError err500 { errBody = utf8LBS "系统繁忙，请稍后重试" }
      Nothing     ->
        throwError err400 { errBody = utf8LBS "必须提供合法的邮箱地址" }

register :: ( MonadError ServerError m
            , Crypto.MonadRandom m
            , MonadPersist backend m
            , ReadEntity UserRegistry (ReaderT backend m)
            , WriteEntity User (ReaderT backend m)
            )
         => Proxy backend
         -> Common.RegisterForm
         -> m NoContent
register p (Common.RegisterForm user password code) = do
    r <- withConn p $ selectFirst
            [ UserRegistryEmail ==. Common._user_email user
            , UserRegistryCode ==. code
            ] []
    r & maybe (throwError err403 { errBody = utf8LBS "验证码已过期" }) (\_ -> do
      createUser p (makeOrdinaryUser user) password
      pure NoContent
      )

validateEmail :: ( MonadError ServerError m
                 , MonadPersist backend m
                 , ReadEntity School (ReaderT backend m)
                 , ReadEntity SchoolDomain (ReaderT backend m)
                 )
              => Proxy backend
              -> Common.EmailAddress
              -> m Text -- ^school name
validateEmail p email =
    withConn p $ (nothingTo404 =<<) $ runMaybeT $ do
      (Entity _ schoolDomain) <- MaybeT $ selectFirst [SchoolDomainDomain ==. domain] []
      let schoolId = schoolDomainSchoolId schoolDomain
      school <- MaybeT $ get schoolId
      pure (schoolName school)
  where
    domain = Common._emailAddress_domain email
    nothingTo404 Nothing = throwError err404
    nothingTo404 (Just x) = pure x

-- 生成验证码
generateCode :: ( MonadRandom m
                , MonadTime m
                , MonadPersist backend m
                , WriteEntity UserRegistry (ReaderT backend m)
                )
             => Proxy backend
             -> Text -- ^email
             -> m Text
generateCode p email = do
    code' <- getRandomR (100000 :: Int, 999999)
    let code = T.pack . show $ code'
    createdAt <- getCurrentTime
    withConn p $ insert_ $ UserRegistry email createdAt code
    pure code

verificationCodeMailBody :: Text
                         -> Text
verificationCodeMailBody code = "验证码：" <> code <> "，30分钟内有效"

createUser :: ( MonadError ServerError m
              , Crypto.MonadRandom m
              , MonadPersist backend m
              , WriteEntity User (ReaderT backend m)
              )
           => Proxy backend
           -> User -- ^user without password
           -> Text -- ^password in plaintext
           -> m ()
createUser p user pwd = do
    hashed <- BCrypt.hashPassword 12 (T.encodeUtf8 pwd)
    let user' = user { userHashedPassword = hashed }
    key <- withConn p $ insertUnique user'
    when (isNothing key) $
      throwError err400 { errBody = utf8LBS "用户已经存在" }

makeOrdinaryUser :: Common.User -> User
makeOrdinaryUser (Common.User email name) = User email name "" RoleUser
