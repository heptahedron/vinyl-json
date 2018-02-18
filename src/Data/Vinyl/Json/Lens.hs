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

class HasJFLens ra rb ia ib | ra -> ia, rb -> ib where
  jfLens :: forall f. (Functor f)
         => (ia -> f ib)
         -> JsonField ra -> f (JsonField rb)
  
instance ( SingOpt opt, SingOpt opt'
         , MaybeWhenOptional opt a ~ mawo
         , MaybeWhenOptional opt' b ~ mbwo
         ) => HasJFLens '(s, opt, a) '(s', opt', b) mawo mbwo where
  jfLens f = case sing @opt of
    SRequired -> \case
      ReqField a -> case sing @opt' of
        SRequired -> ReqField @s' <$> f a
        SOptional -> OptField @s' <$> f a
    SOptional -> \case
      OptField ma -> case sing @opt' of
        SRequired -> ReqField @s' <$> f ma
        SOptional -> OptField @s' <$> f ma

-- TODO explicitly write out the meager 4 instances instead of using
-- singletons if performance for jfLens suffers

-- | 'JsonField' lens that preserves field name and optionality.
jf :: forall a b s opt f ia ib.
      (HasJFLens '(s, opt, a) '(s, opt, b) ia ib, Functor f)
   => (ia -> f ib)
   -> JsonField '(s, opt, a) -> f (JsonField '(s, opt, b))
jf = jfLens @('(s, opt, a)) @('(s, opt, b))

jf_ :: forall opt opt' s a b f ia ib.
       (HasJFLens '(s, opt, a) '(s, opt', b) ia ib, Functor f)
    => (ia -> f ib)
    -> JsonField '(s, opt, a) -> f (JsonField '(s, opt', b))
jf_ = jfLens

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
