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
import qualified Data.Text as T
import           Data.Vinyl (Rec((:&)))
import qualified Data.Vinyl as V
import qualified Data.Vinyl.CoRec as V
import qualified Data.Vinyl.Curry as V
import qualified Data.Vinyl.Functor as V

import qualified Data.Proxy as P
import qualified GHC.TypeLits as TL

withDict :: forall c a r. ((c a) => a -> r) -> V.Dict c a -> r
withDict f (V.Dict a) = f a

withFieldName :: forall s a. (TL.KnownSymbol s, A.ToJSON a) => a -> (T.Text, A.Value)
withFieldName a = (T.pack (TL.symbolVal (P.Proxy @s)), A.toJSON a)

withFieldName' :: forall s a. (TL.KnownSymbol s, A.ToJSON a) => a -> A.Series
withFieldName' a = A.pair (T.pack (TL.symbolVal (P.Proxy @s))) (A.toEncoding a)
