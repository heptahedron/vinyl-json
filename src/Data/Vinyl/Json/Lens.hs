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
  
instance HasJFLens '(s, Required, a) '(s', Required, b) a b where
  jfLens f = \case 
    ReqField a -> ReqField @s' <$> f a

instance HasJFLens '(s, Optional, a) '(s', Required, b) (Maybe a) b where
  jfLens f = \case
    OptField ma -> ReqField @s' <$> f ma

instance HasJFLens '(s, Required, a) '(s', Optional, b) a (Maybe b) where
  jfLens f = \case
    ReqField a -> OptField @s' <$> f a

instance HasJFLens '(s, Optional, a) '(s', Optional, b) (Maybe a) (Maybe b) where
  jfLens f = \case
    OptField a -> OptField @s' <$> f a
     
-- | 'JsonField' lens that preserves field name and optionality.
jf :: forall a b s opt f ia ib.
      (HasJFLens '(s, opt, a) '(s, opt, b) ia ib, Functor f)
   => (ia -> f ib)
   -> JsonField '(s, opt, a) -> f (JsonField '(s, opt, b))
jf = jfLens

-- | 'JsonField' lens that preserves optionality, but lets you specify
-- a new name for the field in the first type argument.
jfRenaming :: forall s' s a b opt f ia ib.
              (HasJFLens '(s, opt, a) '(s', opt, b) ia ib, Functor f)
           => (ia -> f ib)
           -> JsonField '(s, opt, a) -> f (JsonField '(s', opt, b))
jfRenaming = jfLens

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

-- TODO use of another redudant index parameter might quell GHC's
-- worries of UndecidableInstances in 'RElem\'', but is there a reason
-- to avoid using that extension other than the name?

-- | Generalized version of 'Data.Vinyl.Lens.RElem'. Whereas that
-- typeclass constrains the return type of the lens to be the same as
-- the input field type, this one allows you to change it.
class (i ~ V.RIndex r rs) => RElem' (r :: k)    (r' :: k)
                                    (rs :: [k]) (rs' :: [k])
                                    (i :: V.Nat)
                                  | r r' rs i -> rs' where
  rlens' :: Functor g => (f r -> g (f r')) -> V.Rec f rs -> g (V.Rec f rs')

instance RElem' a b (a ': rs) (b ': rs) V.Z where
  rlens' f = \case
    jf :& xs -> (:& xs) <$> f jf

instance ( V.RIndex a (no ': rs) ~ V.S i
         , RElem' a b rs rs' i )
         => RElem' a b (no ': rs) (no ': rs') (V.S i) where
  rlens' f = \case
    jf :& xs -> (jf :&) <$> rlens' f xs

-- | Constraint synonym for use in the record field accessor lens,
-- which indicates the first occurrence (per 'JsonFieldArg') of @s@ in
-- the triple list @rs@, having the form @'(s, opt, a)@, will be
-- replaced by a triple of the form @'(s', opt', b)@, resulting in a
-- type-level list of triples @rs'@.
type ReplacingInJsonRec s s' a b opt opt' rs rs' =
  ( JsonFieldArg s rs ~ '(s, opt, a)
  , RElem' '(s, opt, a) '(s', opt', b)
           rs           rs'
           (V.RIndex '(s, opt, a) rs) )

-- | Lens for accessing a named field within a 'JsonRec'. Best used
-- with @TypeApplications@, like @jRec & jr \@"some_field_name" %~
-- (+4)@.
jr :: forall s s' a b opt rs rs' f.
      ( ReplacingInJsonRec s s' a b opt opt rs rs'
      , Functor f )
   => (JsonField '(s, opt, a) -> f (JsonField '(s', opt, b)))
   -> JsonRec rs -> f (JsonRec rs')
jr f = fmap MkJsonRec . rlens' f . unJsonRec

-- | Convenience lens for accessing the contents of a 'JsonField'
-- /within/ a 'JsonRec' by name, preserving both the name and the
-- optionality.
jrf :: forall s a b opt ia ib rs rs' f.
       ( ReplacingInJsonRec s s a b opt opt rs rs'
       , HasJFLens '(s, opt, a) '(s, opt, b) ia ib
       , Functor f )
    => (ia -> f ib)
    -> JsonRec rs -> f (JsonRec rs')
jrf = jr @s @s @a @b . jf
