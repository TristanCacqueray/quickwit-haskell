module Quickwit.Index where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)

newtype IndexID = IndexID Text
    deriving newtype (FromJSON, ToJSON, Show, Ord, Eq)

-- TODO: perform validation https://quickwit.io/docs/configuration/index-config#index-id
mkIndexID :: Text -> Maybe IndexID
mkIndexID = Just . IndexID
