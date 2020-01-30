module CROE.Backend.Service.User
  ( putProfile
  , getProfile
  , applyCode
  , register
  , validateEmail
  ) where

import           Control.Monad.Trans.Maybe
import           Data.Function                   ((&))
import           Data.Maybe
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import qualified Data.Text.Encoding              as T
import           Database.Persist                (Entity (..), (=.), (==.))
import           Polysemy
import           Servant

import           CROE.Backend.Clock.Class
import           CROE.Backend.Logger.Class
import qualified CROE.Backend.Mail.Class         as Mail
import qualified CROE.Backend.Persist.Class      as Persist
import           CROE.Backend.Random.Class
import           CROE.Backend.Service.Auth.Class
import qualified CROE.Common.API.User            as Common
import qualified CROE.Common.User                as Common
import           CROE.Common.Util                (utf8LBS)

putProfile :: Members [Persist.ConnectionPool, Persist.ReadEntity Persist.User, Persist.WriteEntity Persist.User] r
           => Common.User
           -> Common.User
           -> Sem r (Either ServerError NoContent)
putProfile authUser newUser = do
    Persist.withConn $ \conn ->
      Persist.updateWhere conn
        [ Persist.UserEmail ==. oldEmail]
        [ Persist.UserEmail =. Common._user_email newUser
        , Persist.UserName =. Common._user_name newUser
        ]
    pure (Right NoContent)
  where
    oldEmail = Common._user_email authUser

getProfile :: Common.User
           -> Sem r (Either ServerError Common.User)
getProfile authUser = pure $ Right authUser

applyCode :: Members [ Persist.ConnectionPool
                     , Persist.WriteEntity Persist.User
                     , Persist.WriteEntity Persist.UserRegistry
                     , Logger
                     , Mail.Client
                     , Clock
                     , RandomService
                     ] r
          => Maybe Text
          -> Sem r (Either ServerError NoContent)
applyCode email =
    case email of
      Just email' -> do
        printLog LevelInfo $ email' <> " is registering"
        code <- generateCode email'
        success <- Mail.sendPlainTextMail email' "CROE" "校园代办：注册验证码" (verificationCodeMailBody code)
        if success
        then pure (Right NoContent)
        else pure (Left $ err500 { errBody = utf8LBS "系统繁忙，请稍后重试" })
      Nothing     ->
        pure $ Left $ err400 { errBody = utf8LBS "必须提供合法的邮箱地址" }

register :: Members '[ AuthService
                     , Persist.ConnectionPool
                     , Persist.ReadEntity Persist.UserRegistry
                     , Persist.WriteEntity Persist.User
                     ] r
         => Common.RegisterForm
         -> Sem r (Either ServerError NoContent)
register (Common.RegisterForm user password code) = do
    r <- Persist.withConn $ \conn -> Persist.selectFirst conn
            [ Persist.UserRegistryEmail ==. Common._user_email user
            , Persist.UserRegistryCode ==. code
            ] []
    r & maybe (pure . Left $ err403 { errBody = utf8LBS "验证码已过期" }) (\_ -> do
      result <- createUser (makeOrdinaryUser user) password
      pure $ fmap (const NoContent) result
      )

validateEmail :: Members '[ Persist.ConnectionPool
                          , Persist.ReadEntity Persist.School
                          , Persist.ReadEntity Persist.SchoolDomain
                          ] r
              => Common.EmailAddress
              -> Sem r (Either ServerError Text)
validateEmail email =
    Persist.withConn $ \conn -> (nothingTo404 =<<) $ runMaybeT $ do
      (Entity _ schoolDomain) <- MaybeT $ Persist.selectFirst conn [Persist.SchoolDomainDomain ==. domain] []
      let schoolId = Persist.schoolDomainSchoolId schoolDomain
      school <- MaybeT $ Persist.get conn schoolId
      pure (Persist.schoolName school)
  where
    domain = Common._emailAddress_domain email
    nothingTo404 Nothing  = pure . Left $ err404
    nothingTo404 (Just x) = pure . Right $ x

-- 生成验证码
generateCode :: Members [ RandomService
                        , Clock
                        , Persist.ConnectionPool
                        , Persist.WriteEntity Persist.UserRegistry
                        ] r
             => Text -- ^email
             -> Sem r Text
generateCode email = do
    code' <- getRandomR (100000 :: Int, 999999)
    let code = T.pack . show $ code'
    createdAt <- currentTimeUTC
    Persist.withConn $ \conn -> Persist.insert_ conn $ Persist.UserRegistry email createdAt code
    pure code

verificationCodeMailBody :: Text
                         -> Text
verificationCodeMailBody code = "验证码：" <> code <> "，30分钟内有效"

createUser :: Members '[ AuthService
                       , Persist.ConnectionPool
                       , Persist.WriteEntity Persist.User
                       ] r
           => Persist.User -- ^user without password
           -> Text -- ^password in plaintext
           -> Sem r (Either ServerError ())
createUser user pwd = do
    hashed <- hashPassword (T.encodeUtf8 pwd)
    let user' = user { Persist.userHashedPassword = hashed }
    key <- Persist.withConn $ \conn -> Persist.insertUnique conn user'
    if isNothing key
    then pure . Left $ err400 { errBody = utf8LBS "用户已经存在" }
    else pure . Right $ ()

makeOrdinaryUser :: Common.User -> Persist.User
makeOrdinaryUser (Common.User email name) =
    Persist.User email name "" Persist.RoleUser
