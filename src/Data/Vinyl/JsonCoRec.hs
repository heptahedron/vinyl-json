{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
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
module Data.Vinyl.JsonCoRec where

import           Data.Aeson ((.=), (.:), (.:!))
import qualified Data.Aeson as A
import qualified Data.Aeson.Encoding.Internal as A
import qualified Data.Aeson.TH as A
import qualified Data.Aeson.Types as A
import qualified Data.Text as T
import           Data.Vinyl (Rec((:&)))
import qualified Data.Vinyl as V
import qualified Data.Vinyl.CoRec as V
import qualified Data.Vinyl.Functor as V
import qualified Data.Vinyl.TypeLevel as V

import qualified Data.Proxy as P
import           Control.Applicative ((<|>))

import           Data.Vinyl.Json
import           Data.Vinyl.Json.Class
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
        parser = V.Const . fmap MkJsonCoRec . fmap V.CoRec . fmap V.Identity . A.parseJSON @r

