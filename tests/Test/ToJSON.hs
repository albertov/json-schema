{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.ToJSON (tests) where

import Prelude.Compat

import Data.JSON.Schema.Types
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Text (Text)
import Test.Tasty
import Test.Tasty.HUnit

#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
#else
import qualified Data.HashMap.Strict as H
#endif

-- Helper to decode JSON schema and check structure
decodeSchema :: Aeson.Value -> Maybe Aeson.Object
decodeSchema (Aeson.Object obj) = Just obj
decodeSchema _ = Nothing

-- Helper to get a field from an object
getField :: Text -> Aeson.Object -> Maybe Aeson.Value
#if MIN_VERSION_aeson(2,0,0)
getField k = KeyMap.lookup (Key.fromText k)
#else
getField = H.lookup
#endif

tests :: TestTree
tests = testGroup "ToJSON Schema instances"
  [ testGroup "Boolean schema"
      [ testCase "converts to {\"type\": \"boolean\"}" $ do
          let schema = Boolean
              json = Aeson.toJSON schema
              Just obj = decodeSchema json
          getField "type" obj @?= Just (Aeson.String "boolean")
      ]

  , testGroup "Number schema"
      [ testCase "unbounded number" $ do
          let schema = Number unbounded
              json = Aeson.toJSON schema
              Just obj = decodeSchema json
          getField "type" obj @?= Just (Aeson.String "number")
          getField "minimum" obj @?= Nothing
          getField "maximum" obj @?= Nothing

      , testCase "bounded number with min and max" $ do
          let schema = Number (Bound (Just 0) (Just 100))
              json = Aeson.toJSON schema
              Just obj = decodeSchema json
          getField "type" obj @?= Just (Aeson.String "number")
          getField "minimum" obj @?= Just (Aeson.Number 0)
          getField "maximum" obj @?= Just (Aeson.Number 100)

      , testCase "number with only minimum" $ do
          let schema = Number (Bound (Just 0) Nothing)
              json = Aeson.toJSON schema
              Just obj = decodeSchema json
          getField "minimum" obj @?= Just (Aeson.Number 0)
          getField "maximum" obj @?= Nothing
      ]

  , testGroup "Value (string) schema"
      [ testCase "unbounded string" $ do
          let schema = Value unboundedLength
              json = Aeson.toJSON schema
              Just obj = decodeSchema json
          getField "type" obj @?= Just (Aeson.String "string")
          getField "minLength" obj @?= Nothing

      , testCase "bounded string" $ do
          let schema = Value (LengthBound (Just 1) (Just 50))
              json = Aeson.toJSON schema
              Just obj = decodeSchema json
          getField "minLength" obj @?= Just (Aeson.Number 1)
          getField "maxLength" obj @?= Just (Aeson.Number 50)
      ]

  , testGroup "Object schema"
      [ testCase "empty object" $ do
          let schema = Object []
              json = Aeson.toJSON schema
              Just obj = decodeSchema json
          getField "type" obj @?= Just (Aeson.String "object")

      , testCase "object with required fields" $ do
          let schema = Object
                [ Field "name" True (Value unboundedLength)
                , Field "age" True (Number unbounded)
                ]
              json = Aeson.toJSON schema
              Just obj = decodeSchema json
          case getField "required" obj of
            Just (Aeson.Array arr) -> length arr @?= 2
            _ -> assertFailure "Expected required array"

      , testCase "object with optional fields only" $ do
          let schema = Object
                [ Field "name" False (Value unboundedLength)
                ]
              json = Aeson.toJSON schema
              Just obj = decodeSchema json
          getField "required" obj @?= Nothing
      ]

  , testGroup "Map schema"
      [ testCase "converts to additionalProperties" $ do
          let schema = Map (Number unbounded)
              json = Aeson.toJSON schema
              Just obj = decodeSchema json
          getField "type" obj @?= Just (Aeson.String "object")
          case getField "additionalProperties" obj of
            Just (Aeson.Object props) ->
              getField "type" props @?= Just (Aeson.String "number")
            _ -> assertFailure "Expected additionalProperties object"
      ]

  , testGroup "Array schema"
      [ testCase "unbounded array" $ do
          let schema = Array unboundedLength False (Number unbounded)
              json = Aeson.toJSON schema
              Just obj = decodeSchema json
          getField "type" obj @?= Just (Aeson.String "array")
          getField "uniqueItems" obj @?= Nothing

      , testCase "bounded array with uniqueItems" $ do
          let schema = Array (LengthBound (Just 1) (Just 10)) True (Value unboundedLength)
              json = Aeson.toJSON schema
              Just obj = decodeSchema json
          getField "minItems" obj @?= Just (Aeson.Number 1)
          getField "maxItems" obj @?= Just (Aeson.Number 10)
          getField "uniqueItems" obj @?= Just (Aeson.Bool True)
      ]

  , testGroup "Tuple schema"
      [ testCase "fixed-length array" $ do
          let schema = Tuple [Value unboundedLength, Number unbounded, Boolean]
              json = Aeson.toJSON schema
              Just obj = decodeSchema json
          getField "type" obj @?= Just (Aeson.String "array")
          getField "minItems" obj @?= Just (Aeson.Number 3)
          getField "maxItems" obj @?= Just (Aeson.Number 3)
      ]

  , testGroup "Choice schema"
      [ testCase "converts to anyOf" $ do
          let schema = Choice [Boolean, Number unbounded]
              json = Aeson.toJSON schema
              Just obj = decodeSchema json
          case getField "anyOf" obj of
            Just (Aeson.Array arr) -> length arr @?= 2
            _ -> assertFailure "Expected anyOf array"
      ]

  , testGroup "Constant schema"
      [ testCase "string constant" $ do
          let schema = Constant (Aeson.String "fixed-value")
              json = Aeson.toJSON schema
              Just obj = decodeSchema json
          getField "const" obj @?= Just (Aeson.String "fixed-value")

      , testCase "numeric constant" $ do
          let schema = Constant (Aeson.Number 42)
              json = Aeson.toJSON schema
              Just obj = decodeSchema json
          getField "const" obj @?= Just (Aeson.Number 42)
      ]

  , testGroup "Any schema"
      [ testCase "empty object (no constraints)" $ do
          let schema = Any
              json = Aeson.toJSON schema
              Just obj = decodeSchema json
          obj @?= mempty
      ]

  , testGroup "Nested schemas"
      [ testCase "object with nested object" $ do
          let schema = Object
                [ Field "address" True (Object
                    [ Field "street" True (Value unboundedLength)
                    , Field "city" True (Value unboundedLength)
                    ])
                ]
              json = Aeson.toJSON schema
              Just obj = decodeSchema json
          case getField "properties" obj of
            Just (Aeson.Object props) ->
              case getField "address" props of
                Just (Aeson.Object addrObj) ->
                  getField "type" addrObj @?= Just (Aeson.String "object")
                _ -> assertFailure "Expected address object"
            _ -> assertFailure "Expected properties object"

      , testCase "array of objects" $ do
          let schema = Array unboundedLength False (Object
                [ Field "id" True (Number unbounded)
                , Field "name" True (Value unboundedLength)
                ])
              json = Aeson.toJSON schema
              Just obj = decodeSchema json
          case getField "items" obj of
            Just (Aeson.Object itemsObj) ->
              getField "type" itemsObj @?= Just (Aeson.String "object")
            _ -> assertFailure "Expected items object"
      ]

  , testGroup "schemaToJSONSchema function"
      [ testCase "same as toJSON" $ do
          let schema = Object
                [ Field "name" True (Value unboundedLength)
                , Field "count" True (Number (Bound (Just 0) Nothing))
                ]
              jsonViaToJSON = Aeson.toJSON schema
              jsonViaFunction = schemaToJSONSchema schema
          jsonViaToJSON @?= jsonViaFunction
      ]

  , testGroup "Real-world invoice example"
      [ testCase "complex nested structure" $ do
          let schema = Object
                [ Field "invoice_number" True (Value unboundedLength)
                , Field "issue_date" True (Object
                    [ Field "year" True (Number unbounded)
                    , Field "month" True (Number (Bound (Just 1) (Just 12)))
                    , Field "day" True (Number (Bound (Just 1) (Just 31)))
                    ])
                , Field "vendor" True (Object
                    [ Field "name" True (Value unboundedLength)
                    , Field "tax_id" True (Value unboundedLength)
                    ])
                , Field "line_items" True (Array unboundedLength False (Object
                    [ Field "description" True (Value unboundedLength)
                    , Field "quantity" True (Number (Bound (Just 0) Nothing))
                    , Field "unit_price" True (Number (Bound (Just 0) Nothing))
                    ]))
                , Field "total" True (Number (Bound (Just 0) Nothing))
                ]
              json = Aeson.toJSON schema
              Just obj = decodeSchema json

          getField "type" obj @?= Just (Aeson.String "object")

          -- Check required fields
          case getField "required" obj of
            Just (Aeson.Array arr) -> length arr @?= 5
            _ -> assertFailure "Expected 5 required fields"

          -- Check properties exist
          case getField "properties" obj of
            Just (Aeson.Object props) -> do
              -- invoice_number is a string
              case getField "invoice_number" props of
                Just (Aeson.Object invObj) ->
                  getField "type" invObj @?= Just (Aeson.String "string")
                _ -> assertFailure "Expected invoice_number"

              -- issue_date is an object
              case getField "issue_date" props of
                Just (Aeson.Object dateObj) ->
                  getField "type" dateObj @?= Just (Aeson.String "object")
                _ -> assertFailure "Expected issue_date"

              -- line_items is an array
              case getField "line_items" props of
                Just (Aeson.Object lineObj) -> do
                  getField "type" lineObj @?= Just (Aeson.String "array")
                  case getField "items" lineObj of
                    Just (Aeson.Object itemSchemaObj) ->
                      getField "type" itemSchemaObj @?= Just (Aeson.String "object")
                    _ -> assertFailure "Expected items schema"
                _ -> assertFailure "Expected line_items"
            _ -> assertFailure "Expected properties"
      ]
  ]
