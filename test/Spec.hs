{-# LANGUAGE OverloadedStrings #-}
import           Test.Tasty
import           Test.Tasty.Golden
import qualified Data.ByteString.Lazy as BL
import qualified Data.Aeson as A
import           Lens.Micro

import Data.Vinyl.Json
import Data.Vinyl.Json.Lens

main :: IO ()
main = goldenTests >>= defaultMain

type TestFields = [ "req_int"                  ::! Int
                  , "req_just_int"             ::! Maybe Int
                  , "req_nothing_int"          ::! Maybe Int
                  , "opt_present_bool"         ::? Bool
                  , "opt_missing_bool"         ::? Bool
                  , "opt_present_just_bool"    ::? Maybe Bool
                  , "opt_present_nothing_bool" ::? Maybe Bool
                  , "opt_missing_just_bool"    ::? Maybe Bool
                  , "opt_missing_nothing_bool" ::? Maybe Bool
                  ]

testRecords :: JsonRec TestFields
testRecords = MkJsonRec <$> 
  [  ReqField 3
  :& ReqField (Just 3)
  :& ReqField Nothing
  :& OptField (Just True)
  :& OptField Nothing
  :& OptField (Just (Just True))
  :& OptField (Just Nothing)
  :& OptField Nothing
  :& OptField Nothing
  :& RNil
  ]

goldenTests :: IO TestTree
goldenTests = do
  jsonFilePaths <- findByExtension [".json"] "golden-tests"
  fmap (testGroup "Json  conversion tests") $ forM jsonFiles $ \jsonFile ->
    goldenVsString (takeBaseName jsonFilePath) jsonFilePath
      $ maybe "" A.encode <$> A.decode @
