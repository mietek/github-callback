{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

import Control.Applicative ((<$>))
import Control.Lens ((&), (^?), (.~))
import Control.Monad.IO.Class (liftIO)
import Data.Aeson.Lens (_String, key)
import Data.Reflection (Given, give, given)
import Data.Text (Text)
import Network.Wai.Middleware.RequestLogger (logStdout)
import System.Environment (getEnv, getEnvironment)
import Web.Scotty (scotty)

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import qualified Network.HTTP.Types as H
import qualified Network.Wreq as C
import qualified Web.Scotty as S


redirect :: Text -> S.ActionM ()
redirect url = do
    S.redirect $ LT.fromStrict url


rejectBadRequest :: S.ActionM ()
rejectBadRequest = do
    S.html "<h1>400 Bad Request</h1>"
    S.status H.badRequest400


maybeParam :: (S.Parsable a) => LT.Text -> S.ActionM (Maybe a)
maybeParam name =
    S.rescue (Just <$> S.param name) $ const $
      return Nothing


addQuery :: (H.QueryLike a) => a -> Text -> Text
addQuery query url
    | BS.length str > 0 = T.decodeUtf8 $ BS.concat [T.encodeUtf8 url, sep, str]
    | otherwise         = url
  where
    str = H.renderQuery False $ H.toQuery query
    sep = case T.findIndex (== '?') url of
            Just _  -> "&"
            Nothing -> "?"


data Cfg = Cfg
    { cfgClientId     :: Text
    , cfgClientSecret :: Text
    , cfgCallbackUrl  :: Text
    , cfgTargetUrl    :: Text
    }
  deriving (Show)


postAccessTokenReq :: (Given Cfg) => Text -> IO (Maybe Text, Maybe Text)
postAccessTokenReq code = do
    let opts = C.defaults
          & C.param "client_id"     .~ [cfgClientId given]
          & C.param "client_secret" .~ [cfgClientSecret given]
          & C.param "redirect_uri"  .~ [cfgCallbackUrl given]
          & C.param "code"          .~ [code]
          & C.header "Accept"       .~ ["application/json"]
    resp <- C.postWith opts "https://github.com/login/oauth/access_token" BS.empty
    let maccess = resp ^? C.responseBody . key "access_token" . _String
        mscope  = resp ^? C.responseBody . key "scope"        . _String
    return (maccess, mscope)


handleCallback :: (Given Cfg) => S.ActionM ()
handleCallback = do
    mstate <- maybeParam "state"
    mcode  <- maybeParam "code"
    let base =
          [ ("state" :: Text, ) <$> mstate
          , Just ("vendor", "github")
          ]
        go more = redirect $ addQuery (base ++ more) $ cfgTargetUrl given
    case mcode of
      Nothing   -> go [Just ("error", "no_code")]
      Just code -> do
        (maccess, mscope) <- liftIO $ postAccessTokenReq code
        case maccess of
          Nothing    -> go [Just ("error", "no_token")]
          Just token -> go
            [ Just ("access_token", token)
            , ("scope", ) <$> mscope
            ]


main :: IO ()
main = do
    clientId     <- getEnv "GITHUB_CLIENT_ID"
    clientSecret <- getEnv "GITHUB_CLIENT_SECRET"
    callbackUrl  <- getEnv "CALLBACK_URL"
    targetUrl    <- getEnv "TARGET_URL"
    env          <- getEnvironment
    let port = maybe 8080 read $ lookup "PORT" env
        cfg  = Cfg
          { cfgClientId     = T.pack clientId
          , cfgClientSecret = T.pack clientSecret
          , cfgCallbackUrl  = T.pack callbackUrl
          , cfgTargetUrl    = T.pack targetUrl
          }
    give cfg $ scotty port $ do
      S.middleware logStdout
      S.get        "/callback" handleCallback
      S.notFound   rejectBadRequest
