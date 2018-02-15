module Data.Vinyl.Json.Class where

import qualified Data.Aeson.Types as A
import qualified Data.Text as T

class FieldToJson a where
  fieldToJson     :: a -> (T.Text, A.Value)
  fieldToEncoding :: a -> A.Series

class FieldToMaybeJson r where
  toMaybePair     :: r -> Maybe A.Pair
  toMaybeEncoding :: r -> Maybe A.Series

class FromJsonObj a where
  parseJsonObj :: A.Object -> A.Parser a

