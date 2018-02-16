# vinyl-json

You can probably imagine what this library is for by this point.

Example, taken from the docs:

The following example should illustrate the behavior of 'ReqField's,
'OptField's, and their respective behavior during serialization.

```haskell
import Data.Vinyl
import Data.Vinyl.Json
import Data.ByteString.Lazy as B

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

testRec :: JsonRec TestFields
testRec = MkJsonRec
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

printTestRecEnc :: IO ()
printTestRecEnc = B.putStrLn $ A.encode testRec
```

Here, `printTestRecEnc` should print something like

```json
{ "req_int":3
, "req_just_int":3
, "req_nothing_int":null
, "opt_present_bool":true
, "opt_present_just_bool":true
, "opt_present_nothing_bool":null
}
```

Notice how all the fields with `missing` in their name, whose values
were `Nothing`, are in fact missing from the output, and how the
fields with `present` in their name, whose values were `Just
something`, are present in the output.
