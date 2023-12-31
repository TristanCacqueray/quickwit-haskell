-- | http2 json helpers
module Quickwit.HTTP (
    -- * Client
    APIClient,
    Endpoint,
    withAPIClient,

    -- * Request
    readJSON,
    requestJSON,
    requestContentLength,

    -- * Errors
    APIError (..),
    QIO,
)
where

import Control.Exception qualified as E
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import Data.Bifunctor (first)
import Data.Binary.Builder as Builder
import Data.ByteString.Char8 qualified as C8
import Data.ByteString.Lazy (LazyByteString)
import Data.ByteString.Lazy qualified as LBS
import Network.HTTP.Types
import Network.HTTP2.Client
import Network.Run.TCP (runTCPClient)

data APIError = HttpError HTTP2Error | ApiError (Maybe Status) LazyByteString | DecodeError String
    deriving (Show)

type QIO a = IO (Either APIError a)

type Endpoint = (String, Word)

newtype APIClient = SendRequest (forall b. Request -> (Response -> IO b) -> IO b)

readAll :: Response -> IO LazyByteString
readAll resp = Builder.toLazyByteString <$> go mempty
  where
    go acc =
        getResponseBodyChunk resp >>= \case
            "" -> pure acc
            bs -> go (acc <> Builder.fromByteString bs)

catchHttpError :: QIO a -> QIO a
catchHttpError action = toErr <$> E.try action
  where
    toErr = \case
        Left e -> Left (HttpError e)
        Right x -> x

getResponse :: APIClient -> Request -> QIO Response
getResponse (SendRequest sendRequest) request = catchHttpError $ sendRequest request \resp -> do
    case responseStatus resp of
        Just (Status code _) | code >= 200 && code < 300 -> pure $ Right resp
        mStatus -> Left . ApiError mStatus <$> readAll resp

responseJSON :: (FromJSON a) => Response -> QIO a
responseJSON resp = do
    body <- readAll resp
    pure $ first DecodeError $ eitherDecode body

-- Note: still debating if QIO should be ExceptT...
readJSON :: (FromJSON a) => APIClient -> Request -> QIO a
readJSON client request = runExceptT do
    ExceptT . responseJSON =<< ExceptT (getResponse client request)

requestJSON :: (ToJSON a) => Method -> Path -> RequestHeaders -> a -> Request
requestJSON method path headers (encode -> body) =
    requestContentLength method path (contentType : headers) body
  where
    contentType = ("Content-Type", "application/json")

requestContentLength :: Method -> Path -> RequestHeaders -> LazyByteString -> Request
requestContentLength method path headers body =
    requestBuilder method path (contentLength : headers) (Builder.fromLazyByteString body)
  where
    contentLength = ("Content-Length", LBS.toStrict $ encode $ LBS.length body)

withAPIClient :: Endpoint -> (APIClient -> IO a) -> IO a
withAPIClient (host, show -> port) client = runTCPClient host port runHTTP2Client
  where
    cliconf = defaultClientConfig{authority = C8.pack host}
    runHTTP2Client s =
        E.bracket
            (allocSimpleConfig s 4096)
            freeSimpleConfig
            (\conf -> run cliconf conf (\sendRequest _aux -> client (SendRequest sendRequest)))
