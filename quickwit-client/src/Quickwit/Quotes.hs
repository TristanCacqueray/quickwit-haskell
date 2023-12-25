{-# LANGUAGE TemplateHaskellQuotes #-}
module Quickwit.Quotes (index, field) where

import Quickwit.Mapping
import Quickwit.Index
import Data.Text qualified as T
import QQLiterals (QuasiQuoter, qqLiteral)

eitherMkIndex :: String -> Either String IndexID
eitherMkIndex str = maybe (Left ("Failed to parse IndexID: " ++ str)) Right (mkIndexID (T.pack str))

index :: QuasiQuoter
index = qqLiteral eitherMkIndex 'eitherMkIndex

eitherMkField :: String -> Either String FieldName
eitherMkField str = maybe (Left ("Failed to parse FieldName: " ++ str)) Right (mkFieldName (T.pack str))

field :: QuasiQuoter
field = qqLiteral eitherMkField 'eitherMkField
