{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds #-}
module Data.Vinyl.Json.Internal where

import qualified Data.Aeson as A
import qualified Data.Aeson.Encoding.Internal as A
import qualified Data.Aeson.Types as A
import qualified Data.Text as T
import           Data.Vinyl (Rec((:&)))
import qualified Data.Vinyl as V
import qualified Data.Vinyl.CoRec as V
import qualified Data.Vinyl.Curry as V
import qualified Data.Vinyl.Derived as V
import qualified Data.Vinyl.Functor as V

import qualified Data.Proxy as P
import qualified GHC.TypeLits as TL

withDict :: forall c a r. ((c a) => a -> r) -> V.Dict c a -> r
withDict f (V.Dict a) = f a

withFieldName :: forall s a. (TL.KnownSymbol s, A.ToJSON a) => a -> (T.Text, A.Value)
withFieldName a = (T.pack (TL.symbolVal (P.Proxy @s)), A.toJSON a)

withFieldName' :: forall s a. (TL.KnownSymbol s, A.ToJSON a) => a -> A.Series
withFieldName' a = A.pair (T.pack (TL.symbolVal (P.Proxy @s))) (A.toEncoding a)

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
  fromFieldRec :: V.FieldRec rs -> a
  toFieldRec   :: a             -> V.FieldRec rs
