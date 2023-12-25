module Quickwit (
    APIClient,
    APIError (..),
    Endpoint,
    withAPIClient,

    -- * Ingest API
    Commit (..),
    IngestedData (..),
    ingestData,

    -- * Search API
    SearchQuery (..),
    SearchResponse (..),
    searchData,

    -- * Index API
    IndexID,
    mkIndexID,
    CreateIndex (..),
    module Quickwit.Mapping,
    createIndex,
    getAllIndexes,
    deleteIndex,

    -- * QuasiQuotes
    module Quickwit.Quotes,

    -- * Example usage
    local,
    demo,
) where

import Data.Aeson (FromJSON (..), ToJSON (..), Value, encode, object, withObject, (.:), (.=))
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Int (Int64)
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Vector (Vector)
import Data.Vector qualified as V
import GHC.Generics (Generic)
import GHC.IsList (fromList)
import Network.HTTP.Types
import Network.HTTP2.Client

import Quickwit.HTTP
import Quickwit.Index
import Quickwit.Mapping
import Quickwit.Quotes

quickWitVersion :: Text
quickWitVersion = "0.6"

data CreateIndex = CreateIndex IndexID DocMapping
    deriving (Show)

createIndex :: APIClient -> CreateIndex -> QIO IndexConfig
createIndex client (CreateIndex indexID docMapping) = readJSON client req
  where
    req = requestJSON methodPost "/api/v1/indexes" [] body
    body = object ["version" .= quickWitVersion, "index_id" .= indexID, "doc_mapping" .= docMapping]

data IndexConfig = IndexConfig
    { index_id :: IndexID
    , create_timestamp :: Int64
    , doc_mapping :: DocMapping
    }
    deriving (Generic, Show)

instance FromJSON IndexConfig where
    parseJSON = withObject "Resp" \obj -> do
        cfg <- obj .: "index_config"
        IndexConfig <$> cfg .: "index_id" <*> obj .: "create_timestamp" <*> cfg .: "doc_mapping"

getAllIndexes :: APIClient -> QIO (Vector IndexConfig)
getAllIndexes client = readJSON client req
  where
    req = requestNoBody methodGet "/api/v1/indexes" []

data IndexDeleted = IndexDeleted
    { num_docs :: Word
    , file_size_bytes :: Word
    , file_name :: Text
    }
    deriving (Show, Generic)

instance FromJSON IndexDeleted

indexPath :: IndexID -> ByteString
indexPath (IndexID indexID) = "/api/v1/indexes/" <> encodeUtf8 indexID

deleteIndex :: APIClient -> IndexID -> QIO (Vector IndexDeleted)
deleteIndex client (indexPath -> indexID) = readJSON client req
  where
    req = requestNoBody methodDelete indexID []

newtype IngestedData = IngestedData {num_docs_for_processing :: Word}
    deriving newtype (Show, Eq, Num)
    deriving (Generic)

instance FromJSON IngestedData
instance ToJSON IngestedData where
    toJSON i = object ["num_docs_for_processing" .= i.num_docs_for_processing]

data Commit = CommitAuto | CommitWaitFor | CommitForce

commitText :: Commit -> ByteString
commitText = \case
    CommitAuto -> "auto"
    CommitWaitFor -> "wait_for"
    CommitForce -> "force"

ingestPath :: IndexID -> Maybe Commit -> ByteString
ingestPath (IndexID indexID) mCommit =
    mconcat
        [ "/api/v1/"
        , encodeUtf8 indexID
        , "/ingest"
        , maybe "" (mappend "?commit=" . commitText) mCommit
        ]

ingestData :: (ToJSON a) => APIClient -> IndexID -> Maybe Commit -> Vector a -> QIO IngestedData
ingestData client indexID mCommit xs = readJSON client req
  where
    path = ingestPath indexID mCommit
    req = requestContentLength methodPost path [("Content-Type", "application/x-ndjson")] body
    body = LBS.intercalate "\n" (V.toList $ encode <$> xs)

searchPath :: IndexID -> ByteString
searchPath (IndexID indexID) = mconcat ["/api/v1/", encodeUtf8 indexID, "/search"]

data SearchQuery = SearchQuery
    { query :: Text
    , start_timestamp :: Maybe Int64
    }
    deriving (Show)

instance ToJSON SearchQuery where
    toJSON sq =
        object $
            ["query" .= sq.query, "format" .= ("json" :: Text)]
                <> maybe [] (\v -> ["start_timestamp" .= v]) sq.start_timestamp

data SearchResponse a = SearchResponse
    { hits :: Vector a
    , num_hits :: Word
    , elapsed_time_micros :: Word
    }
    deriving (Show)

instance (FromJSON a) => FromJSON (SearchResponse a) where
    parseJSON = withObject "SearchResponse" \obj ->
        SearchResponse
            <$> obj .: "hits"
            <*> obj .: "num_hits"
            <*> obj .: "elapsed_time_micros"

searchData :: (FromJSON a) => APIClient -> IndexID -> SearchQuery -> QIO (SearchResponse a)
searchData client indexID body = readJSON client req
  where
    req = requestJSON methodPost (searchPath indexID) [] body

demo :: IO ()
demo =
    withAPIClient local \client -> do
        putStrLn "\n[+] List indexes"
        putStrLn . take 240 . show =<< getAllIndexes client

        let
            indexID = [index|testy|]

        putStrLn "\n[+] Cleanup index"
        print =<< deleteIndex client indexID

        putStrLn "\n[+] Setup index"
        let fields =
                fromList [FieldMapping [field|title|] Nothing "text" []]
            mapping = DocMapping fields FieldStrict Nothing
        print =<< createIndex client (CreateIndex indexID mapping)

        putStrLn "\n[+] Indexing"
        let xs =
                fromList
                    [ object ["title" .= ("test" :: Text)]
                    , object ["title" .= ("testy" :: Text)]
                    ]
        print =<< ingestData client indexID (Just CommitForce) xs

        putStrLn "\n[+] Searching"
        let query = SearchQuery "*" Nothing
        print =<< searchData @Value client indexID query

local :: Endpoint
local = ("127.0.0.1", 7280)
