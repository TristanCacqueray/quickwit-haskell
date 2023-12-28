module Log.Backend.Quickwit (
    -- * log-base logger
    QuickwitConfig (..),
    withQuickwitLogger,

    -- * create the index
    ensureIndex,
) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (AsyncCancelled (..), race)
import Control.Concurrent.MVar
import Control.Exception (Handler (..), SomeException, catches)
import Control.Monad (forever, void)
import Control.Monad.IO.Unlift (MonadUnliftIO (withRunInIO))
import Data.Aeson (encode, object, (.=))
import Data.Aeson.Types (Pair)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.IORef
import Data.Text (Text)
import Data.Text.IO qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import Log qualified as L
import Log.Backend.StandardOutput (withStdOutLogger)
import Log.Internal.Logger (withLogger)
import Log.Logger (Logger)
import Quickwit qualified as Q
import System.Timeout (timeout)

data QuickwitConfig = QuickwitConfig
    { endpoint :: Q.Endpoint
    , outerLogger :: Maybe Logger
    }

withQuickwitLogger :: (MonadUnliftIO m) => QuickwitConfig -> (Logger -> m a) -> m a
withQuickwitLogger conf act = withRunInIO \unlift -> do
    agent <- newAgent
    res <- race (runAgent conf agent) do
        timeout 10_000_000 (takeMVar agent.baton) >>= \case
            Just () -> pure ()
            Nothing -> do
                doWarn conf "Failed to initialize logger" []
                error "abort"
        logger <- quickwitLogger agent
        withLogger logger (unlift . act)
    case res of
        Left _ -> error "The impossible has happened: quickwit agent died"
        Right a -> pure a

doWarn :: QuickwitConfig -> Text -> [Pair] -> IO ()
doWarn conf msg xs = case conf.outerLogger of
    Nothing -> T.putStrLn msg
    Just logger -> L.runLogT "quickwit" logger L.LogAttention do
        L.logAttention msg (object ("endpoint" .= endpoint : xs))
  where
    endpoint = fst conf.endpoint <> ":" <> show (snd conf.endpoint)

quickwitLogger :: Agent -> IO Logger
quickwitLogger (Agent _ tx) = do
    -- The bulklogger forward the events to an agent so that the http2 client can be resumed
    L.mkBulkLogger "Quickwit" writeMessages flush
  where
    callAgent :: AgentEvent -> IO ()
    callAgent ev = do
        res <- newEmptyMVar
        putMVar tx (res, ev)
        takeMVar res

    writeMessages :: [L.LogMessage] -> IO ()
    writeMessages xs = callAgent $ EventMessages $ V.fromList xs

    flush :: IO ()
    flush = callAgent EventFlush

newAgent :: IO Agent
newAgent = Agent <$> newEmptyMVar <*> newEmptyMVar

data AgentEvent = EventFlush | EventMessages (Vector L.LogMessage)

data Agent = Agent
    { baton :: MVar ()
    , rx :: MVar (MVar (), AgentEvent)
    }

runAgent :: QuickwitConfig -> Agent -> IO ()
runAgent conf agent = do
    -- 'current' holds the messages being indexed
    current <- newIORef Nothing
    mBatton <- newIORef (Just agent.baton)
    -- keep on reconnecting
    retryOnException $ Q.withAPIClient conf.endpoint \client -> do
        -- setup index
        ensureIndex client
        readIORef mBatton >>= \case
            Nothing -> pure ()
            Just baton -> do
                -- indicate we are ready
                putMVar baton ()
                writeIORef mBatton Nothing

        -- keep on indexing
        forever do
            -- resume pending messages
            readIORef current >>= \case
                Nothing -> pure ()
                Just (res, prev) -> do
                    warn "Resubmitting messages" ["count" .= length prev]
                    writeMessages client prev
                    writeIORef current Nothing
                    putMVar res ()
            (res, ev) <- takeMVar agent.rx
            case ev of
                EventFlush -> flush
                EventMessages xs -> do
                    writeIORef current (Just (res, xs))
                    writeMessages client xs
            writeIORef current Nothing
            putMVar res ()
  where
    writeMessages :: Q.APIClient -> Vector L.LogMessage -> IO ()
    writeMessages client xs = do
        warn "ingest start" ["count" .= length xs, "msg" .= xs]
        Right res <- Q.ingestData client indexID Nothing xs
        warn "ingest done" ["res" .= res]

    flush :: IO ()
    flush = warn "Flushing..." []

    warn = doWarn conf

    retryOnException :: IO () -> IO ()
    retryOnException act = act `catches` [asyncHandler, anyHandler]
      where
        asyncHandler = Handler \AsyncCancelled -> pure ()
        anyHandler = Handler \(ex :: SomeException) -> do
            warn "unexpected error, retrying in 10 seconds" ["error" .= show ex]
            threadDelay 10_000_000
            retryOnException act

indexID :: Q.IndexID
indexID = [Q.index|log-base|]

ensureIndex :: Q.APIClient -> IO ()
ensureIndex client = do
    Q.createIndex client (Q.CreateIndex indexID mapping) >>= \case
        Left (Q.ApiError _ (LBS.toStrict -> bs)) | "already exists" `BS.isInfixOf` bs -> pure ()
        Left err -> error (show err)
        Right _ -> pure ()
    putStrLn "ensure index done"
  where
    timestamp = [Q.field|time|]
    mkEnumText f = Q.FieldMapping f Nothing "text" []
    mkSearchText f = Q.FieldMapping f Nothing "text" []
    fields =
        [ Q.FieldMapping timestamp Nothing "datetime" ["fast" .= True, "input_formats" .= ["iso8601" :: Text], "output_format" .= ("unix_timestamp_millis" :: Text)]
        , mkEnumText [Q.field|component|]
        , mkEnumText [Q.field|level|]
        , mkSearchText [Q.field|message|]
        , Q.FieldMapping [Q.field|data|] Nothing "json" []
        , Q.FieldMapping [Q.field|domain|] Nothing "array<text>" []
        ]
    mapping = Q.DocMapping (V.fromList fields) Q.FieldStrict (Just timestamp)

demo :: IO ()
demo = do
    withStdOutLogger $ \stdoutLogger -> do
        let conf = QuickwitConfig Q.local $ Just stdoutLogger
        withQuickwitLogger conf \qlogger -> do
            L.runLogT "main" (qlogger) L.defaultLogLevel do
                L.logInfo_ "let's go!"
                L.logInfo "a message" $ object ["test" .= True]
                L.logInfo_ "last message"
    putStrLn "Demo completed"
