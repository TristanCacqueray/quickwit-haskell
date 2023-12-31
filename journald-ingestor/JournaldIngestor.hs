module JournaldIngestor where

import Pipes qualified as P
import Pipes.Safe qualified as P
import Systemd.Journal qualified as J

import Data.Aeson (object, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.Types (Pair, Value (Null))
import Data.ByteString (ByteString)
import Data.HashMap.Strict qualified as HM
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Log qualified as L
import Log.Backend.Quickwit qualified as Q
import Log.Backend.StandardOutput qualified as L (withStdOutLogger)
import Quickwit qualified as Q

main :: IO ()
main =
    L.withStdOutLogger $ \stdoutLogger -> do
        let conf = Q.QuickwitConfig Q.local $ Just stdoutLogger
        Q.withQuickwitLogger conf \qlogger -> do
            L.runLogT "main" qlogger L.defaultLogLevel do
                P.runSafeT $
                    P.runEffect consume
  where
    consume :: P.Effect (P.SafeT (L.LogT IO)) ()
    consume = P.for openJournal \entry -> do
        let (lvl, msg, attrs) = processEntry entry
        -- lift twice to go through SafeT and Effect
        P.lift $ P.lift do
            L.logMessage lvl msg (case attrs of [] -> Null; xs -> object xs)

    start = J.FromEnd J.Forwards
    openJournal :: (P.MonadSafe m) => P.Producer' J.JournalEntry m ()
    openJournal = J.openJournal [J.LocalOnly] start Nothing Nothing

type EntryInfo = (L.LogLevel, Text, [Pair])

processEntry :: J.JournalEntry -> EntryInfo
processEntry je = HM.foldrWithKey' go (L.LogInfo, "", []) (J.journalEntryFields je)
  where
    go :: J.JournalField -> ByteString -> EntryInfo -> EntryInfo
    go k (decodeUtf8 -> v) acc@(l, _, xs) = case k of
        "_SYSTEMD_USER_UNIT" -> set "unit"
        "_SELINUX_CONTEXT" -> set "context"
        "_EXE" -> set "exe"
        "_CMDLINE" -> set "cmd"
        "_UID" -> set "uid"
        "_PID" -> set "pid"
        "_SYSTEMD_CGROUP" -> set "cgroup"
        "_SYSTEMD_SLICE" -> set "slice"
        "_RUNTIME_SCOPE" -> set "scope"
        "MESSAGE" -> (l, v, xs)
        "PRIORITY" -> setLevel acc v
        _ -> acc
      where
        set = addAttr acc v
    addAttr :: EntryInfo -> Text -> Text -> EntryInfo
    addAttr (l, m, xs) v k = (l, m, Key.fromText k .= v : xs)

    setLevel :: EntryInfo -> Text -> EntryInfo
    setLevel (_, m, xs) v =
        let lvl = case v of
                -- todo
                "42" -> L.LogAttention
                _ -> L.LogInfo
         in (lvl, m, xs)
