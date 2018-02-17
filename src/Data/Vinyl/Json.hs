{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
{- |
Module      : Data.Vinyl.Json
Description : 'JsonRec' record type, for JSON serialization,
              where keys may be omitted.
Copyright   : (c) Braxton Spence, 2018
License     : MIT
Maintainer  : ~@braxton.codes
Stability   : experimental

The following example should illustrate the behavior of 'ReqField's,
'OptField's, and their respective behavior during serialization.

> import Data.Vinyl
> import Data.Vinyl.Json
> import Data.ByteString.Lazy as B
>
> type TestFields = [ "req_int"                  ::! Int
>                   , "req_just_int"             ::! Maybe Int
>                   , "req_nothing_int"          ::! Maybe Int
>                   , "opt_present_bool"         ::? Bool
>                   , "opt_missing_bool"         ::? Bool
>                   , "opt_present_just_bool"    ::? Maybe Bool
>                   , "opt_present_nothing_bool" ::? Maybe Bool
>                   , "opt_missing_just_bool"    ::? Maybe Bool
>                   , "opt_missing_nothing_bool" ::? Maybe Bool
>                   ]
> 
> testRec :: JsonRec TestFields
> testRec = MkJsonRec
>         $  ReqField 3
>         :& ReqField (Just 3)
>         :& ReqField Nothing
>         :& OptField (Just True)
>         :& OptField Nothing
>         :& OptField (Just (Just True))
>         :& OptField (Just Nothing)
>         :& OptField Nothing
>         :& OptField Nothing
>         :& RNil
> 
> printTestRecEnc :: IO ()
> printTestRecEnc = B.putStrLn $ A.encode testRec

Here, @printTestRecEnc@ should print something like

> { "req_int":3
> , "req_just_int":3
> , "req_nothing_int":null
> , "opt_present_bool":true
> , "opt_present_just_bool":true
> , "opt_present_nothing_bool":null
> }

Notice how all the fields with @missing@ in their name, whose values
were @Nothing@, are in fact missing from the output, and how the
fields with @present@ in their name, whose values were @Just
something@, are present in the output.

-}
module Data.Vinyl.Json
  ( (::?), (::!)
  , Optionality
  , JsonField
  , JsonRec
  )
where

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

import           Data.Maybe (maybeToList)
import qualified Data.Proxy as P
import           Data.Semigroup ((<>))
import qualified GHC.TypeLits as TL

import           Data.Vinyl.Json.Class
import           Data.Vinyl.Json.Internal

infixr 0 ::?

-- | Convenient infix operator for JSON fields that are optional,
-- i.e. might not be present in a given object.
type (::?) s t = '(s, Optional, t)

infixr 0 ::!

-- | Convenient infix operator for JSON fields that are required.
type (::!) s t = '(s, Required, t)

-- | Whether a field is required or optional.
data Optionality = Required | Optional

-- | Singleton for the 'Optionality' type, not intended for external
-- use.
data SOptionality opt where
  SRequired :: SOptionality Required
  SOptional :: SOptionality Optional

-- | One-off singleton class to get 'SOptionality' values, not
-- intended for external user.
class SingOpt opt where
  sing :: SOptionality opt

instance SingOpt Required where sing = SRequired
instance SingOpt Optional where sing = SOptional

-- | Corresponds to a key-value pair in JSON-encoded data. @JsonField
-- '(s, opt, a)@ is the field with key @s@, having 'Optionality'
-- @opt@, and a value of type @a@. If a field is allowed to be @null@,
-- but the key itself must still be present in the JSON, then it would
-- be of type @JsonField '("field_name", Required, Maybe a)@. If the
-- field need not be included in the JSON, it has 'Optionality'
-- 'Optional'.
data JsonField :: (TL.Symbol, Optionality, *) -> * where
  -- | Constructor for fields whose key must be present in the encoded JSON.
  ReqField :: (TL.KnownSymbol s) => !a         -> JsonField '(s, Required, a)
  -- | Constructor for fields whose key may be missing from the encoded JSON.
  OptField :: (TL.KnownSymbol s) => !(Maybe a) -> JsonField '(s, Optional, a)

type family MaybeWhenOptional r t where
  MaybeWhenOptional Required t = t
  MaybeWhenOptional Optional t = Maybe t

-- | Lens for accessing the contents of a 'JsonField'. When the
-- 'JsonField' is 'Optional', these contents will be wrapped in a
-- 'Maybe'.
jf :: forall f s opt a b. (Functor f)
    => (MaybeWhenOptional opt a -> f (MaybeWhenOptional opt b))
    -> JsonField '(s, opt, a) -> f (JsonField '(s, opt, b))
jf f = \case 
  ReqField a  -> ReqField <$> f a
  OptField ma -> OptField <$> f (ma :: Maybe a)

type family JsonFieldArg s rs where
  JsonFieldArg s ('(s, opt, a)':rs)  = '(s, opt, a)
  JsonFieldArg s ('(s', opt, a)':rs) = JsonFieldArg s rs
  JsonFieldArg s '[]
    = TL.TypeError (       TL.Text "Could not find "
                   TL.:<>: TL.ShowType s
                   TL.:<>: TL.Text " in json record type."
                   )

class (i ~ V.RIndex r rs) => RElem' (f :: k -> *)
                                    (r :: k)    (r' :: k)
                                    (rs :: [k]) (rs' :: [k])
                                    (i :: V.Nat) | r r' rs -> rs' where
  rlens' :: Functor g => (f r -> g (f r')) -> V.Rec f rs -> g (V.Rec f rs')

instance RElem' JsonField  '(s, opt, a)         '(s, opt, b)
                          ('(s, opt, a) ': rs) ('(s, opt, b) ': rs)
                          V.Z where
  rlens' :: Functor g
         => (JsonField '(s, opt, a) -> g (JsonField '(s, opt, b)))
         -> V.Rec JsonField ('(s, opt, a) ': rs)
         -> g (V.Rec JsonField ('(s, opt, b) ': rs))
  rlens' f = \case
    jf :& xs -> (:& xs) <$> f jf

instance ( V.RIndex '(s, opt, a) (no ': rs) ~ V.S i
         , RElem' JsonField '(s, opt, a) '(s, opt, b) rs rs' i )
         => RElem' JsonField
                   '(s, opt, a) '(s, opt, b)
                   (no ': rs)   (no ': rs')
                   (V.S i) where
  rlens' :: Functor g
         => (JsonField '(s, opt, a) -> g (JsonField '(s, opt, b)))
         -> V.Rec JsonField (no ': rs)
         -> g (V.Rec JsonField (no ': rs'))
  rlens' f = \case
    jf :& xs -> (jf :&) <$> rlens' f xs

-- | Newtype for Vinyl records with 'JsonField' types, to avoid
-- attaching any instances directly to 'V.Rec'.
newtype JsonRec rs = MkJsonRec { unJsonRec :: V.Rec JsonField rs }

type family OptionalityOfIn s a rs where
  OptionalityOfIn s a ('(s, opt, a) ': rs)   = opt
  OptionalityOfIn s a ('(s', opt, a') ': rs) = OptionalityOfIn s a rs
  
type ReplacingInJsonRec s a b opt rs rs' =
  ( JsonFieldArg s rs ~ '(s, opt, a)
  , RElem' JsonField '(s, opt, a) '(s, opt, b)
                      rs           rs'
                      (V.RIndex '(s, opt, a) rs))

jr :: forall s a b opt rs rs' f.
       (ReplacingInJsonRec s a b opt rs rs', Functor f)
    => (JsonField '(s, opt, a) -> f (JsonField '(s, opt, b)))
    -> JsonRec rs -> f (JsonRec rs')
jr f = fmap MkJsonRec . rlens' f . unJsonRec

type TestFields = [ "foo" ::! Int
                  , "bar" ::? Bool
                  ]

type TestRec = JsonRec [ "foo" ::! Int
                       , "bar" ::? Bool
                       ]

testRec :: TestRec
testRec = MkJsonRec $ ReqField 3 :& OptField Nothing :& V.RNil

(^.) :: forall s a. s -> ((a -> V.Const a a) -> s -> V.Const a s) -> a
s ^. l = V.getConst $ l V.Const s

(%~) :: forall s t a b. ((a -> V.Identity b) -> s -> V.Identity t) -> (a -> b) -> s -> t
l %~ f = V.getIdentity . l (V.Identity . f) 

l .~ a = l %~ (const a)

instance (A.ToJSON a) => FieldToMaybeJson (JsonField '(s, opt, a)) where
  toMaybePair (ReqField a)      = Just (withFieldName @s a)
  toMaybePair (OptField ma)     = withFieldName @s <$> ma
  toMaybeEncoding (ReqField a)  = Just (withFieldName' @s a)
  toMaybeEncoding (OptField ma) = withFieldName' @s <$> ma

instance (V.RecAll JsonField rs FieldToMaybeJson) => A.ToJSON (JsonRec rs) where
  toJSON = A.object
         . concat
         . V.recordToList
         . V.rmap (  withDict (V.Const . maybeToList . toMaybePair)
                   . V.getCompose)
         . V.reifyConstraint (P.Proxy @FieldToMaybeJson)
         . unJsonRec

  toEncoding = A.pairs
             . foldr (<>) mempty
             . concat
             . V.recordToList
             . V.rmap (  withDict (V.Const . maybeToList . toMaybeEncoding)
                       . V.getCompose)
             . V.reifyConstraint (P.Proxy @FieldToMaybeJson)
             . unJsonRec

instance FromJsonObj (JsonRec '[]) where
  parseJsonObj = const $ return $ MkJsonRec V.RNil

instance ( FromJsonObj (JsonRec rs), A.FromJSON a
         , TL.KnownSymbol s, SingOpt opt)
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

instance (Show a) => Show (JsonField '(s, opt, a)) where
  show = \case
    ReqField a  -> fName <> " !-> " <> show a
    OptField ma -> fName <> " ?-> " <> case ma of
      Just a  -> show a
      Nothing -> "[missing]"
    where
      fName :: (TL.KnownSymbol s) => String
      fName = TL.symbolVal (P.Proxy @s)
  
instance (V.RecAll JsonField rs Show) => Show (JsonRec rs) where
  show = show . unJsonRec

instance IsFieldRec (JsonRec '[]) '[] where
  fromFieldRec = const $ MkJsonRec V.RNil
  toFieldRec = const V.RNil . unJsonRec

instance (IsFieldRec (JsonRec jrs) rs)
         => IsFieldRec (JsonRec ('(s, Optional, t)':jrs))
                       ('(s, Maybe t)':rs) where
  fromFieldRec ((V.Field ma) :& xs)
    = MkJsonRec $ OptField @s ma :& (unJsonRec $ fromFieldRec xs)
  toFieldRec (MkJsonRec ((OptField ma) :& xs))
    = V.Field @s ma :& toFieldRec (MkJsonRec xs)

instance (IsFieldRec (JsonRec jrs) rs)
         => IsFieldRec (JsonRec ('(s, Required, t)':jrs))
                       ('(s, t)':rs) where
  fromFieldRec ((V.Field a) :& xs)
    = MkJsonRec $ ReqField @s a :& (unJsonRec $ fromFieldRec xs)
  toFieldRec (MkJsonRec ((ReqField a) :& xs))
    = V.Field @s a :& toFieldRec (MkJsonRec xs)
