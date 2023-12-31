module Quickwit.Mapping (
    DocMapping (..),
    DocumentMode (..),

    -- * Field
    FieldName,
    mkFieldName,
    FieldMapping (..),
) where

import Data.Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types
import Data.Text (Text)
import Data.Vector (Vector)

newtype FieldName = FieldName Text
    deriving newtype (FromJSON, ToJSON, Show, Ord, Eq)

-- TODO: perform validation
mkFieldName :: Text -> Maybe FieldName
mkFieldName = Just . FieldName

data FieldMapping = FieldMapping
    { name :: FieldName
    , description :: Maybe Text
    , typ :: Text
    , attrs :: [Pair]
    }
    deriving (Show, Ord, Eq)

instance ToJSON FieldMapping where
    toJSON fm = object (baseAttrs <> fm.attrs)
      where
        -- base attrs are shared by all field types.
        baseAttrs =
            [ "name" .= fm.name
            , "description" .= fm.description
            , "type" .= fm.typ
            ]

instance FromJSON FieldMapping where
    parseJSON = withObject "FieldMapping" \obj -> do
        FieldMapping
            <$> obj .: "name"
            <*> obj .:? "description"
            <*> obj .: "type"
            <*> pure (filter removeKeys $ KM.toList obj)
      where
        removeKeys (k, _v) = k `notElem` ["name", "description", "type"]

data DynamicMapping = TODO
    deriving (Show, Ord, Eq)

instance FromJSON DynamicMapping where
    parseJSON = undefined

instance ToJSON DynamicMapping where
    toJSON = undefined

data DocumentMode
    = FieldDynamic DynamicMapping
    | FieldLenient
    | FieldStrict
    deriving (Show, Ord, Eq)

fieldModeName :: DocumentMode -> Text
fieldModeName = \case
    FieldDynamic _ -> "dynamic"
    FieldLenient -> "lenient"
    FieldStrict -> "strict"

parseDocumentMode :: Object -> Text -> Parser DocumentMode
parseDocumentMode obj = \case
    "dynamic" -> FieldDynamic <$> obj .: "dynamic_mapping"
    "lenient" -> pure FieldLenient
    "strict" -> pure FieldStrict
    mode -> fail ("Unknown mode: " <> show mode)

data DocMapping = DocMapping
    { field_mappings :: Vector FieldMapping
    , mode :: DocumentMode
    , timestamp_field :: Maybe FieldName
    }
    deriving (Show, Ord, Eq)

instance ToJSON DocMapping where
    toJSON dm = object (modAttrs baseAttrs)
      where
        baseAttrs =
            maybe [] (\v -> ["timestamp_field" .= v]) dm.timestamp_field
                <> [ "field_mappings" .= dm.field_mappings
                   , "mode" .= fieldModeName dm.mode
                   ]
        modAttrs = case dm.mode of
            FieldDynamic ds -> ("dynamic_mapping" .= ds :)
            _ -> id

instance FromJSON DocMapping where
    parseJSON = withObject "DocMapping" \obj ->
        DocMapping
            <$> obj .: "field_mappings"
            <*> (parseDocumentMode obj =<< obj .: "mode")
            <*> obj .: "timestamp_field"
