{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
Module      : Data.Vinyl.Json.Simple
Description : Wrapper around 'Data.Vinyl.FieldRec' for simple (de-)serialization.
Copyright   : (c) Braxton Spence, 2018
License     : MIT
Maintainer  : ~@braxton.codes
Stability   : experimental

If you know the JSON encoding of some data will /always/ have all the
fields indicated, it may be more performant to use this \"simple\"
JSON record type, where there is no affordance for missing
fields. Additionally, by having the implementation be nothing more
than a wrapper around 'Data.Vinyl.Derived.FieldRec', a more trivial
isomorphism exists between 'Data.Vinyl.Derived.FieldRec' and
'Data.Vinyl.Json.Simple.JsonRec''.
-}
module Data.Vinyl.Json.Simple where

import           Data.Aeson ((.=), (.:), (.:!))
import qualified Data.Aeson as A
import qualified Data.Aeson.Encoding.Internal as A
import qualified Data.Aeson.TH as A
import qualified Data.Aeson.Types as A
import qualified Data.Text as T
import           Data.Vinyl (Rec((:&)))
import qualified Data.Vinyl as V
import qualified Data.Vinyl.Functor as V
import qualified Data.Vinyl.TypeLevel as V

import qualified Data.Proxy as P
import qualified GHC.TypeLits as TL
import           Data.Semigroup ((<>))

import           Data.Vinyl.Json.Internal

-- | \"Simple\" JSON field type. Unlike 'Data.Vinyl.Json.JsonField', all
-- 'JsonField'' are implicitly required. This allows for a trivial
-- isomorphism between 'Data.Vinyl.FieldRec' and
-- 'Data.Vinyl.Json.Simple.JsonRec''.
newtype JsonField' (r :: (TL.Symbol, *)) = MkJsonField' { unJsonField' :: V.ElField r }
  
-- | \"Simple\" JSON record type, where all fields are implicitly required.
newtype JsonRec' rs = MkJsonRec' { unJsonRec' :: V.Rec JsonField' rs }

instance (A.ToJSON a) => FieldToJson (JsonField' '(s, a)) where
  fieldToJson (MkJsonField' (V.Field a)) = withFieldName @s a
  fieldToEncoding (MkJsonField' (V.Field a)) = withFieldName' @s a

instance (V.RecAll JsonField' rs FieldToJson) => A.ToJSON (JsonRec' rs) where
  toJSON = A.object
         . V.recordToList
         . V.rmap (withDict (V.Const . fieldToJson) . V.getCompose)
         . V.reifyConstraint (P.Proxy @FieldToJson)
         . unJsonRec'

  toEncoding = A.pairs
             . foldr (<>) A.Empty
             . V.recordToList
             . V.rmap (withDict (V.Const . fieldToEncoding) . V.getCompose)
             . V.reifyConstraint (P.Proxy @FieldToJson)
             . unJsonRec'

instance FromJsonObj (JsonRec' '[]) where
  parseJsonObj = const $ return $ MkJsonRec' V.RNil
  
instance (A.FromJSON a, FromJsonObj (JsonRec' rs), TL.KnownSymbol s)
  => FromJsonObj (JsonRec' ('(s, a)':rs)) where
  parseJsonObj o = do
   a <- o .: fName
   MkJsonRec' rs <- parseJsonObj @(JsonRec' rs) o 
   return $ MkJsonRec' $ (MkJsonField' $ V.Field @s a) :& rs
   where
     fName = T.pack $ TL.symbolVal $ P.Proxy @s
  
instance (FromJsonObj (JsonRec' rs)) => A.FromJSON (JsonRec' rs) where
  parseJSON = A.withObject "JsonRec'" $ parseJsonObj

instance (Show a) => Show (JsonField' '(s, a)) where
  show = show . unJsonField'

instance (V.RecAll JsonField' rs Show) => Show (JsonRec' rs) where
  show = show . unJsonRec'

instance IsFieldRec (JsonRec' rs) rs where
  fromFieldRec = MkJsonRec' . V.rmap MkJsonField'
  toFieldRec   = V.rmap unJsonField' . unJsonRec'

-- _jsonRec' :: Iso' (V.FieldRec rs) (JsonRec' rs)
-- _jsonRec' = iso toJsonRec' fromJsonRec'
