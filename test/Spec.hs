{-# LANGUAGE OverloadedStrings #-}
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.Hspec
import qualified Data.ByteString.Lazy as BL
import qualified Data.Aeson as A
import           Lens.Micro

import Data.Vinyl
import Data.Vinyl.Json
import Data.Vinyl.Json.Lens

main :: IO ()
main = do
  jsonRecTests <- sequence [goldenTests, unitTests]
  defaultMain (testGroup "JsonRec tests" jsonRecTests)

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

testRecord :: JsonRec TestFields
testRecord =  MkJsonRec
           $  ReqField 3
           :& ReqField (Just 3)
           :& ReqField Nothing
           :& OptField (Just True)
           :& OptField Nothing
           :& OptField (Just (Just True))
           :& OptField (Just Nothing)
           :& OptField Nothing
           :& OptField Nothing
           :& RNil

inGoldenTestDirectory :: FilePath -> FilePath
inGoldenTestDirectory = ("test/golden-tests/" ++)

goldenTests :: IO TestTree
goldenTests = 
  return $ testGroup "Encoding tests"
    [ goldenVsString ("TestFields encoding")
        (inGoldenTestDirectory "test-fields-encoding.json")
        $ return $ A.encode testRecord
    ]

unitTests :: IO TestTree
unitTests = do
  testSpec "Decoding tests" $ describe "JSON decoding" $ do
    it "decodes into an object identical to the one that was encoded" $ do
      mTestRec <- A.decode @(JsonRec TestFields)
                  <$> BL.readFile (inGoldenTestDirectory
                                     "test-fields-encoding.json")
      mTestRec `shouldBe` (Just testRecord)
