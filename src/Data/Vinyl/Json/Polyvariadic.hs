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
module Data.Vinyl.Json.Polyvariadic where

import qualified Data.Aeson as A
import qualified Data.Aeson.Encoding.Internal as A
import qualified Data.Text as T
import           Data.Vinyl (Rec((:&)))
import qualified Data.Vinyl as V
import qualified Data.Vinyl.CoRec as V
import qualified Data.Vinyl.Curry as V
import qualified Data.Vinyl.Functor as V

import qualified GHC.TypeLits as TL
import qualified Data.Proxy as P

import Data.Vinyl.Json
import Data.Vinyl.JsonCoRec

class RReverseApp (rs :: [k]) (acc :: [k]) (res :: [k]) | rs acc -> res where
  rReverseApp :: V.Rec f rs -> V.Rec f acc -> V.Rec f res

instance RReverseApp '[] acc acc where
  rReverseApp _ acc = acc

instance (RReverseApp rs (r ': acc) res) => RReverseApp (r ': rs) acc res where
  rReverseApp (x :& xs) acc = rReverseApp xs (x :& acc)

class RReverse (rs :: [k]) (sr :: [k]) | rs -> sr, sr -> rs where
  rReverse :: V.Rec f rs -> V.Rec f sr

instance (RReverseApp rs '[] sr, RReverseApp sr '[] rs)
         => RReverse rs sr where
  rReverse rs = rReverseApp rs V.RNil

class BuildRec (f :: k -> *) (rs :: [k]) (res :: *) where
  buildRec' :: V.Rec f rs -> res

instance RReverse rs sr => BuildRec f rs (V.Rec f sr) where
  buildRec' rs = rReverse rs

instance (BuildRec f (r ': rs) res)
         => BuildRec f rs (f r -> res) where
  buildRec' rs = \fa -> buildRec' (fa :& rs)

class BuildMatcher a rs res where
  buildMatcher' :: V.Rec (V.Handler a) rs -> res

instance (RReverse rs sr) => BuildMatcher a rs (V.Rec (V.Handler a) sr) where
  buildMatcher' rs = rReverse rs

instance (RReverse rs sr) => BuildMatcher a rs (V.CoRec V.Identity sr -> a)  where
  buildMatcher' rs = flip V.match $ rReverse rs

instance (BuildMatcher a (r ': rs) res)
         => BuildMatcher a rs ((r -> a) -> res) where
  buildMatcher' rs = \h -> buildMatcher' (V.H h :& rs)

type family Handlers' rs a where
  Handlers' (r ': rs) a = (r -> a) ': Handlers' rs a
  Handlers' '[]       a = '[]

mkRec :: forall (f :: k -> *) (rs :: [k]).
         (BuildRec f '[] (V.CurriedF f rs (V.Rec f rs)))
      => V.CurriedF f rs (V.Rec f rs)
mkRec = (buildRec' :: V.Rec f '[] -> V.CurriedF f rs (V.Rec f rs)) V.RNil

mkMatcher :: forall rs a.
             (BuildMatcher a '[] (V.Curried (Handlers' rs a)
                                            (V.CoRec V.Identity rs -> a)))
          => V.Curried (Handlers' rs a) (V.CoRec V.Identity rs -> a)
mkMatcher = buildMatcher' @a V.RNil
            
class BuildJRec rs res opts where
  mkJRec' :: JsonRec rs -> res

instance (BuildJRec ('(s, Required, a) ': rs) res opts)
         => BuildJRec rs (a -> res) (Required ': opts) where
  mkJRec' (MkJsonRec rs) = \a -> mkJRec' @_ @res @opts (MkJsonRec $ ReqField @s a :& rs)

instance (BuildJRec ('(s, Optional, a) ': rs) res opts)
         => BuildJRec rs (Maybe a -> res) (Optional ': opts) where
  mkJRec' (MkJsonRec rs) = \a -> mkJRec' @_ @res @opts (MkJsonRec $ OptField @s a :& rs)

instance (RReverse rs sr) => BuildJRec rs (JsonRec sr) opts where
  mkJRec' (MkJsonRec rs) = MkJsonRec $ rReverse rs

type family CurriedJ rs res where
  CurriedJ ('(s, Required, a) ': rs) res = a       -> CurriedJ rs res
  CurriedJ ('(s, Optional, a) ': rs) res = Maybe a -> CurriedJ rs res
  CurriedJ '[]                       res = res

type family OptList rs where
  OptList ('(s, opt, a) ': rs) = opt ': OptList rs
  OptList '[]                  = '[]

mkJRec :: forall rs. (BuildJRec '[] (CurriedJ rs (JsonRec rs)) (OptList rs))
       => CurriedJ rs (JsonRec rs)
mkJRec = mkJRec' @_ @_ @(OptList rs) $ MkJsonRec V.RNil

instance (RReverse rs sr) => BuildMatcher a rs (JsonCoRec sr -> a)  where
  buildMatcher' rs = flip V.match (rReverse rs) . unJsonCoRec

matchJ :: forall rs a.
          (BuildMatcher a '[] (V.Curried (Handlers' rs a)
                                         (JsonCoRec rs -> a)))
       => V.Curried (Handlers' rs a) (JsonCoRec rs -> a)
matchJ = buildMatcher' @a V.RNil
