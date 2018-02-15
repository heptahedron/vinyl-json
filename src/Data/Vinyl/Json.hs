{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
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
module Data.Vinyl.Json where

import           Data.Aeson ((.=), (.:), (.:!))
import qualified Data.Aeson as A
import qualified Data.Aeson.Encoding.Internal as A
import qualified Data.Aeson.TH as A
import qualified Data.Aeson.Types as A
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text as T
import           Data.Vinyl (Rec((:&)))
import qualified Data.Vinyl as V
import qualified Data.Vinyl.Functor as V
import qualified Data.Vinyl.TypeLevel as V

import           Data.Maybe (maybeToList)
import qualified Data.Proxy as P
import           Data.Semigroup ((<>))
import qualified GHC.TypeLits as TL

import           Data.Vinyl.Json.Class
import           Data.Vinyl.Json.Internal

infixr 0 ::?

type (::?) s t = '(s, Optional, t)

infixr 0 ::!

type (::!) s t = '(s, Required, t)

data Optionality = Required | Optional

data SOptionality opt where
  SRequired :: SOptionality Required
  SOptional :: SOptionality Optional

class SingI opt where
  sing :: SOptionality opt

instance SingI Required where sing = SRequired
instance SingI Optional where sing = SOptional

data JsonField :: (TL.Symbol, Optionality, *) -> * where
  ReqField :: (TL.KnownSymbol s) => !a         -> JsonField '(s, Required, a)
  OptField :: (TL.KnownSymbol s) => !(Maybe a) -> JsonField '(s, Optional, a)

newtype JsonRec rs = MkJsonRec { unJsonRec :: V.Rec JsonField rs }
  
instance (A.ToJSON a) => FieldToMaybeJson (JsonField '(s, opt, a)) where
  toMaybePair (ReqField a)      = Just (withFieldName @s a)
  toMaybePair (OptField ma)     = withFieldName @s <$> ma
  toMaybeEncoding (ReqField a)  = Just (withFieldName' @s a)
  toMaybeEncoding (OptField ma) = withFieldName' @s <$> ma

instance (V.RecAll JsonField rs FieldToMaybeJson) => A.ToJSON (JsonRec rs) where
  toJSON = A.object
         . concat
         . V.recordToList
         . V.rmap (withDict (V.Const . maybeToList . toMaybePair) . V.getCompose)
         . V.reifyConstraint (P.Proxy @FieldToMaybeJson)
         . unJsonRec

  toEncoding = A.pairs
             . foldr (<>) mempty
             . concat
             . V.recordToList
             . V.rmap (withDict (V.Const . maybeToList . toMaybeEncoding) . V.getCompose)
             . V.reifyConstraint (P.Proxy @FieldToMaybeJson)
             . unJsonRec

instance FromJsonObj (JsonRec '[]) where
  parseJsonObj = const $ return $ MkJsonRec V.RNil

instance (FromJsonObj (JsonRec rs), A.FromJSON a, TL.KnownSymbol s, SingI opt)
  => FromJsonObj (JsonRec ('(s, opt, a)':rs)) where
  parseJsonObj o = case sing @opt of
    SRequired -> do
      a <- o .: fName
      rs <- unJsonRec <$> parseJsonObj @(JsonRec rs) o
      return $ MkJsonRec $ (ReqField @s a) :& rs
    SOptional -> do
      ma <- o .:! fName
      rs <- unJsonRec <$> parseJsonObj @(JsonRec rs) o
      return $ MkJsonRec $ (OptField @s ma) :& rs
    where
      fName = T.pack $ TL.symbolVal $ P.Proxy @s

instance (FromJsonObj (JsonRec rs)) => A.FromJSON (JsonRec rs) where
  parseJSON = A.withObject "JsonRec" $ parseJsonObj
  
instance (A.ToJSON (JsonRec rs)) => Show (JsonRec rs) where
  show = B.unpack . A.encode  

type family JsonFieldName r where
  JsonFieldName '(s, opt, t) = s

type family ElFieldName r where
  ElFieldName '(s, t) = s

type family ToElField r where
  ToElField '(s, Required, t) = '(s, t)
  ToElField '(s, Optional, t) = '(s, Maybe t)

type family ToFieldRec rs where
  ToFieldRec (r ':rs) = (ToElField r)':ToFieldRec rs
  ToFieldRec '[]      = '[]

type family ToJsonField r where
  -- As far as I can tell, there's not a great way to make this mirror
  -- ToElField.
  ToJsonField '(s, t) = '(s, Required, t)

type family ToJsonRec r where
  ToJsonRec (r ':rs) = (ToJsonField r)':ToJsonRec rs
  ToJsonRec '[]      = '[]

toElField :: forall r. JsonField r -> V.ElField (ToElField r)
toElField (ReqField a) = V.Field @(JsonFieldName r) a
toElField (OptField a) = V.Field @(JsonFieldName r) a

toFieldRec :: forall rs. JsonRec rs -> V.FieldRec (ToFieldRec rs)
toFieldRec (MkJsonRec (x :& xs)) = toElField x :& toFieldRec (MkJsonRec xs)
toFieldRec (MkJsonRec V.RNil)    = V.RNil

toJsonField :: forall r. V.ElField r -> JsonField (ToJsonField r)
toJsonField (V.Field a) = ReqField @(ElFieldName r) a

toJsonRec :: forall rs. V.FieldRec rs -> JsonRec (ToJsonRec rs)
toJsonRec (x :& xs) = MkJsonRec $ (toJsonField x) :& (unJsonRec $ toJsonRec xs)
toJsonRec V.RNil    = MkJsonRec $ V.RNil
