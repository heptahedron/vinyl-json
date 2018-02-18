{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{- |
Module      : Data.Vinyl.Json.Class
Copyright   : (c) Braxton Spence, 2018
License     : MIT
Maintainer  : ~@braxton.codes
Stability   : experimental

Classes used internally for serialization.
-}
module Data.Vinyl.Json.Class where

import qualified Data.Aeson.Types as A
import qualified Data.Text as T
import           Data.Vinyl
import           Data.Vinyl.Lens
import           Data.Vinyl.TypeLevel

-- | For use in the serialization of 'Data.Vinyl.Json.Simple.JsonRec''.
class FieldToJson a where
  fieldToJson     :: a -> (T.Text, A.Value)
  fieldToEncoding :: a -> A.Series

-- | For use in the serialization of 'Data.Vinyl.Json.JsonRec', where
-- a given field may be optional, and thus might not be present in the
-- final encoding.
class FieldToMaybeJson r where
  toMaybePair     :: r -> Maybe A.Pair
  toMaybeEncoding :: r -> Maybe A.Series

-- | For use in deserialization of both JSON record types, allows us
-- to forego repackaging what we know to be a
-- 'Data.Aeson.Types.Object' as a 'Data.Aeson.Types.Value' between the
-- different fields in a record.
class FromJsonObj a where
  parseJsonObj :: A.Object -> A.Parser a

-- | For conversion to and from 'Data.Vinyl.Derived.FieldRec'.
class IsFieldRec a rs | a -> rs where
  fromFieldRec :: FieldRec rs -> a
  toFieldRec   :: a           -> FieldRec rs
