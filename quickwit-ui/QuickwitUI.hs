module Main where

import Control.Concurrent (threadDelay)
import Data.Aeson (ToJSON, Value, encode)
import Data.ByteString.Lazy (toStrict)
import Data.Generics.Labels ()
import Data.Text (Text, pack)
import Data.Text.Encoding (decodeUtf8)
import Data.Vector qualified as V
import GHC.Generics (Generic)
import Monomer
import Quickwit qualified as Q
import System.Environment (getEnv)

data AppModel = AppModel
    { search :: Text
    , fetching :: Bool
    , results :: [Text]
    }
    deriving (Eq, Show, Generic)

initModel :: AppModel
initModel = AppModel "*" False []

data AppEvent
    = AppInit
    | AppQuit
    | AppUpdate
    | AppDisplay [Text]
    deriving (Eq, Show)

buildUI ::
    WidgetEnv AppModel AppEvent ->
    AppModel ->
    WidgetNode AppModel AppEvent
buildUI _wenv model = keystroke [("Esc", AppQuit), ("Enter", AppUpdate)] widgetTree
  where
    widgetTree = vstack [searchBar, searchResult] `styleBasic` [padding 10]
    searchBar = hstack [label "Search", spacer, textField #search]
    searchResult = vstack $ map label model.results

handleEvent ::
    WidgetEnv AppModel AppEvent ->
    WidgetNode AppModel AppEvent ->
    AppModel ->
    AppEvent ->
    [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
    AppInit -> [Task fetchResult]
    AppQuit -> [exitApplication]
    AppUpdate | not model.fetching -> [Model (model{fetching = True}), Task fetchResult]
    AppDisplay xs -> [Model (model{fetching = False, results = xs})]
    _ -> []
  where
    encodeJSON :: (ToJSON a) => a -> Text
    encodeJSON = decodeUtf8 . toStrict . encode
    fetchResult :: IO AppEvent
    fetchResult = Q.withAPIClient Q.local \client -> do
        eResp <- Q.searchData client [Q.index|log-base|] $ Q.SearchQuery model.search Nothing
        pure $ AppDisplay $ case eResp of
            Left err -> [pack (show err)]
            Right resp -> V.toList $ encodeJSON @Value <$> resp.hits

main :: IO ()
main = do
    robotFont <- pack <$> getEnv "ROBOTO_TTF"
    startApp initModel handleEvent buildUI $ config robotFont
  where
    config f =
        [ appWindowTitle "Quickwit UI"
        , appTheme darkTheme
        , appFontDef "Regular" f
        , appInitEvent AppInit
        ]
