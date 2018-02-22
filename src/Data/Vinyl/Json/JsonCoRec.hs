{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
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
Module      : Data.Vinyl.Json.JsonCoRec
Description : 'JsonCoRec' record type, for JSON serialization of sums of types.
Copyright   : (c) Braxton Spence, 2018
License     : MIT
Maintainer  : ~@braxton.codes
Stability   : experimental

-}
module Data.Vinyl.Json.JsonCoRec where

import           Data.Aeson ((.=), (.:), (.:!))
import qualified Data.Aeson as A
import qualified Data.Aeson.Encoding.Internal as A
import qualified Data.Aeson.TH as A
import qualified Data.Aeson.Types as A
import qualified Data.Text as T
import           Data.Vinyl (Rec((:&)))
import qualified Data.Vinyl as V
import qualified Data.Vinyl.CoRec as V
import qualified Data.Vinyl.Curry as V
import qualified Data.Vinyl.Functor as V
import qualified Data.Vinyl.TypeLevel as V

import qualified Data.Proxy as P
import           Control.Applicative ((<|>))

import           Data.Vinyl.Json.Internal

newtype JsonCoRec rs = MkJsonCoRec { unJsonCoRec :: V.CoRec V.Identity rs }

instance (V.AllAllSat '[A.ToJSON] rs, V.RecApplicative rs)
         => A.ToJSON (JsonCoRec rs) where
  toJSON     = V.onField (P.Proxy @('[A.ToJSON])) A.toJSON     . unJsonCoRec
  toEncoding = V.onField (P.Proxy @('[A.ToJSON])) A.toEncoding . unJsonCoRec

class (V.RElem r rs (V.RIndex r rs)) => IsIn rs r
instance (V.RElem r rs (V.RIndex r rs)) => IsIn rs r

instance (V.AllAllSat '[A.FromJSON, IsIn rs] rs, V.RecApplicative rs)
         => A.FromJSON (JsonCoRec rs) where
    parseJSON v = foldr (<|>) (fail "Could not parse any variant of JsonRec")
                $ V.recordToList
                $ V.reifyDicts @_ @_ @_ @rs
                  (P.Proxy @('[A.FromJSON, IsIn rs])) (parser v)
      where
        parser :: forall r rs b. (IsIn rs r, A.FromJSON r)
               => A.Value -> V.Const (A.Parser (JsonCoRec rs)) r
        parser = V.Const . fmap mkJCoRec . A.parseJSON @r

mkJCoRec :: forall rs r. (V.RElem r rs (V.RIndex r rs)) => r -> JsonCoRec rs
mkJCoRec = MkJsonCoRec . V.CoRec . V.Identity
            
