{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module AOC.API (
  mkAocClient,
) where

import AOC.Types 
import Control.Monad ((>=>))
import Data.ByteString qualified as BS
import Data.Proxy (Proxy (..))
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Time.Clock (addUTCTime, getCurrentTime)
import GHC.Conc (atomically, newTVar)
import Network.HTTP.Client (Cookie (..), createCookieJar)
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Media ((//), (/:))
import Servant.API
import Servant.Client hiding ((//), (/:))

mkAocClient :: String -> Int -> Int -> IO (IO T.Text, Submission -> IO Answer)
mkAocClient session year day = do
  env <- mkAocEnv session
  let aocInput :<|> aocSubmit = aocClient year day
      f = flip runClientM env >=> pure . either (error . show) id
      aocInput' = f aocInput
      aocSubmit' = f . aocSubmit
  return (aocInput', aocSubmit')

mkAocEnv :: String -> IO ClientEnv
mkAocEnv session = do
  current <- getCurrentTime
  manager <- newTlsManager
  let
    base = BaseUrl Https "adventofcode.com" 443 ""
    env = mkClientEnv manager base
    year = 60 * 60 * 24 * 365
    expiry = addUTCTime year current
    cookie =
      Cookie
        { cookie_name = "session",
          cookie_value = T.encodeUtf8 . T.pack $ session,
          cookie_expiry_time = expiry,
          cookie_domain = T.encodeUtf8 . T.pack $ baseUrlHost base,
          cookie_path = "/",
          cookie_creation_time = current,
          cookie_last_access_time = current,
          cookie_persistent = True,
          cookie_host_only = True,
          cookie_secure_only = True,
          cookie_http_only = True
        }
  cookies <- atomically $ newTVar (createCookieJar [cookie])
  return $ env {cookieJar = Just cookies}

aocClient :: Int -> Int -> (ClientM T.Text :<|> (Submission -> ClientM Answer))
aocClient = client (Proxy @API)

type API =
  Capture "year" Int :>
    ("day" :> Capture "day" Int :>
      (
        -- GET /:year/day/:day/input
        ("input" :> Get '[RawPlainText] T.Text) :<|>
        -- POST /:year/day/:day/answer level=<1|2>&answer=_
        ("answer" :> ReqBody '[FormUrlEncoded] Submission :> Post '[HTML] Answer)
      )
    )

-- This is silly: https://github.com/haskell-servant/servant/issues/1002
data RawPlainText

instance Accept RawPlainText where
  contentType _ = "text" // "plain"

instance MimeUnrender RawPlainText T.Text where
  mimeUnrender _ = Right . T.decodeUtf8 . BS.toStrict

-- TODO implement marshalling from HTML to 'Day', 'Year', and 'Answer'
-- types which extracts yearly calendars, day prompts, and
-- pre-existing correct answer submissions via scraping the received
-- HTML.

data HTML

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

data Answer = Correct | Low | High | Empty

instance MimeUnrender HTML Answer where
  mimeUnrender _ bs
    | "correct" `BS.isInfixOf` bs' = Right Correct
    | "low" `BS.isInfixOf` bs' = Right Low
    | "high" `BS.isInfixOf` bs' = Right High
    | "provide an answer" `BS.isInfixOf` bs' = Right Empty
    | otherwise = Left "Unknown answer response"
    where
      bs' = BS.toStrict bs
