module Main where

import Data.Aeson (ToJSON, Value, encode)
import Data.ByteString.Lazy (toStrict)
import Data.Generics.Labels ()
import Data.Text (Text, pack)
import Data.Text.Encoding (decodeUtf8)
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
    widgetTree = vstack [searchBar, searchResult]
    searchBar = hstack [label "Search", spacer, textField #search] `styleBasic` [padding 10]
    searchResult = vscroll_ [] $ vstack $ map resultRow model.results
    resultRow result = label_ result [multiline] `styleBasic` [paddingB 2]

handleEvent ::
    WidgetEnv AppModel AppEvent ->
    WidgetNode AppModel AppEvent ->
    AppModel ->
    AppEvent ->
    [AppEventResponse AppModel AppEvent]
handleEvent _wenv _node model evt = case evt of
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
        eResp <- Q.searchDataList client [Q.index|log-base|] $ Q.SearchQuery model.search Nothing
        pure $ AppDisplay $ case eResp of
            Left err -> [pack (show err)]
            Right resp -> encodeJSON @Value <$> resp.hits

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
