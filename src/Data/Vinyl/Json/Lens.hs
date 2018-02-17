{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
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
module Data.Vinyl.Json.Lens where

import           Data.Vinyl (Rec((:&)))
import qualified Data.Vinyl as V
import qualified Data.Vinyl.Functor as V
import qualified Data.Vinyl.TypeLevel as V

import qualified GHC.TypeLits as TL

import           Data.Vinyl.Json
import           Data.Vinyl.Json.Class
import           Data.Vinyl.Json.Internal

-- | Lens for accessing the contents of a 'JsonField'. When the
-- 'JsonField' is 'Optional', these contents will be wrapped in a
-- 'Maybe'.
jf :: forall f s opt a b. (Functor f)
   => (MaybeWhenOptional opt a -> f (MaybeWhenOptional opt b))
   -> JsonField '(s, opt, a) -> f (JsonField '(s, opt, b))
jf f = \case 
  ReqField a  -> ReqField <$> f a
  OptField ma -> OptField <$> f (ma :: Maybe a)

-- | Type family that finds the first instance of the symbol @s@ in
-- the list of @(TL.Symbol, Optionality, *)@ triples and returns the
-- triple in which it was found. Meant to be used with the record
-- field accessor lenses and @TypeApplications@, so the field can be
-- specified by simply adding @\@"field_name"@ to the invocation of
-- the lens.
type family JsonFieldArg s rs where
  JsonFieldArg s ('(s, opt, a)':rs)  = '(s, opt, a)
  JsonFieldArg s ('(s', opt, a)':rs) = JsonFieldArg s rs
  JsonFieldArg s '[]
    = TL.TypeError (       TL.Text "Could not find "
                   TL.:<>: TL.ShowType s
                   TL.:<>: TL.Text " in json record type."
                   )

-- | Constraint synonym for use in the record field accessor lens,
-- which indicates the first occurrence (per 'JsonFieldArg') of @s@ in
-- the triple list @rs@, having the form @'(s, opt, a)@, will be
-- replaced by a triple of the form @'(s, opt', b)@, resulting in a
-- type-level list of triples @rs'@.
type ReplacingInJsonRec s a b opt opt' rs rs' =
  ( JsonFieldArg s rs ~ '(s, opt, a)
  , RElem' '(s, opt, a) '(s, opt', b)
                      rs           rs'
                      (V.RIndex '(s, opt, a) rs))

-- TODO use of another redudant index parameter might quell GHC's
-- worries of UndecidableInstances in 'RElem\'', but is there a reason
-- to avoid using that extension other than the name?

-- | Lens for accessing a named field within a 'JsonRec'. Best used
-- with @TypeApplications@, like @jRec & jr \@"some_field_name" %~
-- (+4)@.
jr :: forall s a b opt rs rs' f.
       (ReplacingInJsonRec s a b opt opt rs rs', Functor f)
    => (JsonField '(s, opt, a) -> f (JsonField '(s, opt, b)))
    -> JsonRec rs -> f (JsonRec rs')
jr f = fmap MkJsonRec . rlens' f . unJsonRec

{-
toRequired :: forall s a b opt rs rs' f.
              (ReplacingInJsonRec s a b opt Required rs rs', Functor f)
           => (MaybeWhenOptional opt a -> f b)
           -> JsonField '(s, opt, a) -> f (JsonField '(s, Required, b))
toRequired f = \case
  ReqField a  -> ReqField <$> f a
  OptField ma -> ReqField <$> f ma
-}
-- (^.) :: forall s a. s -> ((a -> V.Const a a) -> s -> V.Const a s) -> a
-- s ^. l = V.getConst $ l V.Const s
-- 
-- (%~) :: forall s t a b. ((a -> V.Identity b) -> s -> V.Identity t) -> (a -> b) -> s -> t
-- l %~ f = V.getIdentity . l (V.Identity . f) 
-- 
-- l .~ a = l %~ (const a)
