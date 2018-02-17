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

-- | Generalized version of 'Data.Vinyl.Lens.RElem'. Whereas that
-- typeclass constrains the return type of the lens to be the same as
-- the input field type, /this/ one allows you to change it.
class (i ~ RIndex r rs) => RElem' (r :: k)    (r' :: k)
                                  (rs :: [k]) (rs' :: [k])
                                  (i :: Nat)
                                  | r r' rs i -> rs' where
  rlens' :: Functor g => (f r -> g (f r')) -> Rec f rs -> g (Rec f rs')

instance RElem' a b (a ': rs) (b ': rs) Z where
  rlens' f = \case
    jf :& xs -> (:& xs) <$> f jf

instance ( RIndex a (no ': rs) ~ S i
         , RElem' a b rs rs' i )
         => RElem' a b (no ': rs) (no ': rs') (S i) where
  rlens' f = \case
    jf :& xs -> (jf :&) <$> rlens' f xs
