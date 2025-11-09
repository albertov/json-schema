{-# LANGUAGE
    FlexibleInstances
  , OverloadedStrings
  , ScopedTypeVariables
  , TypeSynonymInstances
  , CPP
  #-}
-- | Types for defining JSON schemas.
module Data.JSON.Schema.Types
  ( JSONSchema (..)
  , Schema (..)
  , Field (..)
  , Bound (..)
  , LengthBound (..)
  , unbounded
  , unboundedLength
  , schemaToJSONSchema
  ) where

import Prelude.Compat

import Data.Fixed
import Data.Int
import Data.Maybe
import Data.Proxy.Compat
import Data.Scientific
import Data.String.Compat
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Vector (Vector)
import Data.Word.Compat
import qualified Data.Aeson.Types    as Aeson
import qualified Data.HashMap.Strict as H
import qualified Data.Map            as M
import qualified Data.Set            as S
import qualified Data.Text.Lazy      as L
import qualified Data.Vector         as V

#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
#endif

-- | A schema for a JSON value.
data Schema =
    Choice [Schema]      -- ^ A choice of multiple values, e.g. for sum types.
  | Object [Field]       -- ^ A JSON object.
  | Map    Schema        -- ^ A JSON object with arbitrary keys.
  | Array LengthBound Bool Schema
                         -- ^ An array. The LengthBound represent the
                         -- lower and upper bound of the array
                         -- size. The value 'unboundedLength' indicates no bound.
                         -- The boolean denotes whether items have
                         -- to be unique.
  | Tuple [Schema]       -- ^ A fixed-length tuple of different values.
  | Value LengthBound    -- ^ A string. The LengthBound denote the lower and
                         -- upper bound of the length of the string. The
                         -- value 'unboundedLength' indicates no bound.
  | Boolean              -- ^ A Bool.
  | Number Bound         -- ^ A number. The Bound denote the lower and
                         -- upper bound on the value. The value 'unbounded'
                         -- indicates no bound.
  | Constant Aeson.Value -- ^ A Value that never changes. Can be
                         -- combined with Choice to create enumerables.
  | Any                  -- ^ Any value is allowed.
  deriving (Eq, Show)

-- | A type for bounds on number domains. Use Nothing when no lower or upper bound makes sense
data Bound = Bound
  { lower :: Maybe Int
  , upper :: Maybe Int
  } deriving (Eq, Show)

-- | A type for bounds on lengths for strings and arrays. Use Nothing when no lower or upper bound makes sense
data LengthBound = LengthBound
  { lowerLength :: Maybe Int
  , upperLength :: Maybe Int
  } deriving (Eq, Show)

unbounded :: Bound
unbounded = Bound Nothing Nothing

integralSchema :: forall a. (Bounded a, Integral a) => Proxy a -> Schema
integralSchema _ =
  Number $ Bound (Just $ fromIntegral (minBound::a))
                 (Just $ fromIntegral (maxBound::a))

unboundedLength :: LengthBound
unboundedLength = LengthBound Nothing Nothing

-- | A field in an object.
data Field = Field { key :: Text, required :: Bool, content :: Schema } deriving (Eq, Show)

-- | Class representing JSON schemas
class JSONSchema a where
  schema :: Proxy a -> Schema

instance JSONSchema () where
  schema _ = Constant Aeson.Null

instance JSONSchema Int where
  schema _ = Number unbounded

instance JSONSchema Integer where
  schema _ = Number unbounded

instance JSONSchema Int8 where
  schema = integralSchema

instance JSONSchema Int16 where
  schema = integralSchema

instance JSONSchema Int32 where
  schema _ = Number unbounded

instance JSONSchema Int64 where
  schema _ = Number unbounded

instance JSONSchema Word where
  schema _ = Number (Bound (Just 0) Nothing)

instance JSONSchema Word8 where
  schema = integralSchema

instance JSONSchema Word16 where
  schema = integralSchema

instance JSONSchema Word32 where
  schema _ = Number (Bound (Just 0) Nothing)

instance JSONSchema Word64 where
  schema _ = Number (Bound (Just 0) Nothing)

instance JSONSchema Float where
  schema _ = Number unbounded

instance JSONSchema Double where
  schema _ = Number unbounded

instance HasResolution a => JSONSchema (Fixed a) where
  schema _ = Number unbounded

instance JSONSchema Scientific where
  schema _ = Number unbounded

instance JSONSchema Bool where
  schema _ = Boolean

instance JSONSchema Text where
  schema _ = Value unboundedLength

instance JSONSchema L.Text where
  schema _ = Value unboundedLength

instance JSONSchema a => JSONSchema (Maybe a) where
  schema p = Choice [Object [Field "Just" True $ schema $ fmap fromJust p], Object [Field "Nothing" True (Constant Aeson.Null)]]

instance JSONSchema a => JSONSchema [a] where
  schema = Array unboundedLength False . schema . fmap head

instance JSONSchema a => JSONSchema (Vector a) where
  schema = Array unboundedLength False . schema . fmap V.head

instance (IsString k, JSONSchema v) => JSONSchema (M.Map k v) where
  schema = Map . schema . fmap (head . M.elems)

instance (IsString k, JSONSchema v) => JSONSchema (H.HashMap k v) where
  schema = Map . schema . fmap (head . H.elems)

instance JSONSchema UTCTime where
  schema _ = Value LengthBound { lowerLength = Just 20, upperLength = Just 33 }

instance JSONSchema a => JSONSchema (S.Set a) where
  schema = Array unboundedLength True . schema . fmap S.findMin

instance JSONSchema Aeson.Value where
  schema _ = Any

instance (JSONSchema a, JSONSchema b) => JSONSchema (a, b) where
  schema s = Tuple
    [ schema . fmap fst $ s
    , schema . fmap snd $ s
    ]

instance (JSONSchema a, JSONSchema b, JSONSchema c) => JSONSchema (a, b, c) where
  schema s = Tuple
    [ schema . fmap (\(a,_,_) -> a) $ s
    , schema . fmap (\(_,b,_) -> b) $ s
    , schema . fmap (\(_,_,c) -> c) $ s
    ]

instance (JSONSchema a, JSONSchema b, JSONSchema c, JSONSchema d) => JSONSchema (a, b, c, d) where
  schema s = Tuple
    [ schema . fmap (\(a,_,_,_) -> a) $ s
    , schema . fmap (\(_,b,_,_) -> b) $ s
    , schema . fmap (\(_,_,c,_) -> c) $ s
    , schema . fmap (\(_,_,_,d) -> d) $ s
    ]

instance (JSONSchema a, JSONSchema b, JSONSchema c, JSONSchema d, JSONSchema e) => JSONSchema (a, b, c, d, e) where
  schema s = Tuple
    [ schema . fmap (\(a,_,_,_,_) -> a) $ s
    , schema . fmap (\(_,b,_,_,_) -> b) $ s
    , schema . fmap (\(_,_,c,_,_) -> c) $ s
    , schema . fmap (\(_,_,_,d,_) -> d) $ s
    , schema . fmap (\(_,_,_,_,e) -> e) $ s
    ]

instance (JSONSchema a, JSONSchema b, JSONSchema c, JSONSchema d, JSONSchema e, JSONSchema f) => JSONSchema (a, b, c, d, e, f) where
  schema s = Tuple
    [ schema . fmap (\(a,_,_,_,_,_) -> a) $ s
    , schema . fmap (\(_,b,_,_,_,_) -> b) $ s
    , schema . fmap (\(_,_,c,_,_,_) -> c) $ s
    , schema . fmap (\(_,_,_,d,_,_) -> d) $ s
    , schema . fmap (\(_,_,_,_,e,_) -> e) $ s
    , schema . fmap (\(_,_,_,_,_,f) -> f) $ s
    ]

instance (JSONSchema a, JSONSchema b, JSONSchema c, JSONSchema d, JSONSchema e, JSONSchema f, JSONSchema g) => JSONSchema (a, b, c, d, e, f, g) where
  schema s = Tuple
    [ schema . fmap (\(a,_,_,_,_,_,_) -> a) $ s
    , schema . fmap (\(_,b,_,_,_,_,_) -> b) $ s
    , schema . fmap (\(_,_,c,_,_,_,_) -> c) $ s
    , schema . fmap (\(_,_,_,d,_,_,_) -> d) $ s
    , schema . fmap (\(_,_,_,_,e,_,_) -> e) $ s
    , schema . fmap (\(_,_,_,_,_,f,_) -> f) $ s
    , schema . fmap (\(_,_,_,_,_,_,g) -> g) $ s
    ]

instance (JSONSchema a, JSONSchema b, JSONSchema c, JSONSchema d, JSONSchema e, JSONSchema f, JSONSchema g, JSONSchema h) => JSONSchema (a, b, c, d, e, f, g, h) where
  schema s = Tuple
    [ schema . fmap (\(a,_,_,_,_,_,_,_) -> a) $ s
    , schema . fmap (\(_,b,_,_,_,_,_,_) -> b) $ s
    , schema . fmap (\(_,_,c,_,_,_,_,_) -> c) $ s
    , schema . fmap (\(_,_,_,d,_,_,_,_) -> d) $ s
    , schema . fmap (\(_,_,_,_,e,_,_,_) -> e) $ s
    , schema . fmap (\(_,_,_,_,_,f,_,_) -> f) $ s
    , schema . fmap (\(_,_,_,_,_,_,g,_) -> g) $ s
    , schema . fmap (\(_,_,_,_,_,_,_,h) -> h) $ s
    ]

instance (JSONSchema a, JSONSchema b, JSONSchema c, JSONSchema d, JSONSchema e, JSONSchema f, JSONSchema g, JSONSchema h, JSONSchema i) => JSONSchema (a, b, c, d, e, f, g, h, i) where
  schema s = Tuple
    [ schema . fmap (\(a,_,_,_,_,_,_,_,_) -> a) $ s
    , schema . fmap (\(_,b,_,_,_,_,_,_,_) -> b) $ s
    , schema . fmap (\(_,_,c,_,_,_,_,_,_) -> c) $ s
    , schema . fmap (\(_,_,_,d,_,_,_,_,_) -> d) $ s
    , schema . fmap (\(_,_,_,_,e,_,_,_,_) -> e) $ s
    , schema . fmap (\(_,_,_,_,_,f,_,_,_) -> f) $ s
    , schema . fmap (\(_,_,_,_,_,_,g,_,_) -> g) $ s
    , schema . fmap (\(_,_,_,_,_,_,_,h,_) -> h) $ s
    , schema . fmap (\(_,_,_,_,_,_,_,_,i) -> i) $ s
    ]

instance (JSONSchema a, JSONSchema b, JSONSchema c, JSONSchema d, JSONSchema e, JSONSchema f, JSONSchema g, JSONSchema h, JSONSchema i, JSONSchema j) => JSONSchema (a, b, c, d, e, f, g, h, i, j) where
  schema s = Tuple
    [ schema . fmap (\(a,_,_,_,_,_,_,_,_,_) -> a) $ s
    , schema . fmap (\(_,b,_,_,_,_,_,_,_,_) -> b) $ s
    , schema . fmap (\(_,_,c,_,_,_,_,_,_,_) -> c) $ s
    , schema . fmap (\(_,_,_,d,_,_,_,_,_,_) -> d) $ s
    , schema . fmap (\(_,_,_,_,e,_,_,_,_,_) -> e) $ s
    , schema . fmap (\(_,_,_,_,_,f,_,_,_,_) -> f) $ s
    , schema . fmap (\(_,_,_,_,_,_,g,_,_,_) -> g) $ s
    , schema . fmap (\(_,_,_,_,_,_,_,h,_,_) -> h) $ s
    , schema . fmap (\(_,_,_,_,_,_,_,_,i,_) -> i) $ s
    , schema . fmap (\(_,_,_,_,_,_,_,_,_,j) -> j) $ s
    ]

instance (JSONSchema a, JSONSchema b, JSONSchema c, JSONSchema d, JSONSchema e, JSONSchema f, JSONSchema g, JSONSchema h, JSONSchema i, JSONSchema j, JSONSchema k) => JSONSchema (a, b, c, d, e, f, g, h, i, j, k) where
  schema s = Tuple
    [ schema . fmap (\(a,_,_,_,_,_,_,_,_,_,_) -> a) $ s
    , schema . fmap (\(_,b,_,_,_,_,_,_,_,_,_) -> b) $ s
    , schema . fmap (\(_,_,c,_,_,_,_,_,_,_,_) -> c) $ s
    , schema . fmap (\(_,_,_,d,_,_,_,_,_,_,_) -> d) $ s
    , schema . fmap (\(_,_,_,_,e,_,_,_,_,_,_) -> e) $ s
    , schema . fmap (\(_,_,_,_,_,f,_,_,_,_,_) -> f) $ s
    , schema . fmap (\(_,_,_,_,_,_,g,_,_,_,_) -> g) $ s
    , schema . fmap (\(_,_,_,_,_,_,_,h,_,_,_) -> h) $ s
    , schema . fmap (\(_,_,_,_,_,_,_,_,i,_,_) -> i) $ s
    , schema . fmap (\(_,_,_,_,_,_,_,_,_,j,_) -> j) $ s
    , schema . fmap (\(_,_,_,_,_,_,_,_,_,_,k) -> k) $ s
    ]

instance (JSONSchema a, JSONSchema b, JSONSchema c, JSONSchema d, JSONSchema e, JSONSchema f, JSONSchema g, JSONSchema h, JSONSchema i, JSONSchema j, JSONSchema k, JSONSchema l) => JSONSchema (a, b, c, d, e, f, g, h, i, j, k, l) where
  schema s = Tuple
    [ schema . fmap (\(a,_,_,_,_,_,_,_,_,_,_,_) -> a) $ s
    , schema . fmap (\(_,b,_,_,_,_,_,_,_,_,_,_) -> b) $ s
    , schema . fmap (\(_,_,c,_,_,_,_,_,_,_,_,_) -> c) $ s
    , schema . fmap (\(_,_,_,d,_,_,_,_,_,_,_,_) -> d) $ s
    , schema . fmap (\(_,_,_,_,e,_,_,_,_,_,_,_) -> e) $ s
    , schema . fmap (\(_,_,_,_,_,f,_,_,_,_,_,_) -> f) $ s
    , schema . fmap (\(_,_,_,_,_,_,g,_,_,_,_,_) -> g) $ s
    , schema . fmap (\(_,_,_,_,_,_,_,h,_,_,_,_) -> h) $ s
    , schema . fmap (\(_,_,_,_,_,_,_,_,i,_,_,_) -> i) $ s
    , schema . fmap (\(_,_,_,_,_,_,_,_,_,j,_,_) -> j) $ s
    , schema . fmap (\(_,_,_,_,_,_,_,_,_,_,k,_) -> k) $ s
    , schema . fmap (\(_,_,_,_,_,_,_,_,_,_,_,l) -> l) $ s
    ]

instance (JSONSchema a, JSONSchema b, JSONSchema c, JSONSchema d, JSONSchema e, JSONSchema f, JSONSchema g, JSONSchema h, JSONSchema i, JSONSchema j, JSONSchema k, JSONSchema l, JSONSchema m) => JSONSchema (a, b, c, d, e, f, g, h, i, j, k, l, m) where
  schema s = Tuple
    [ schema . fmap (\(a,_,_,_,_,_,_,_,_,_,_,_,_) -> a) $ s
    , schema . fmap (\(_,b,_,_,_,_,_,_,_,_,_,_,_) -> b) $ s
    , schema . fmap (\(_,_,c,_,_,_,_,_,_,_,_,_,_) -> c) $ s
    , schema . fmap (\(_,_,_,d,_,_,_,_,_,_,_,_,_) -> d) $ s
    , schema . fmap (\(_,_,_,_,e,_,_,_,_,_,_,_,_) -> e) $ s
    , schema . fmap (\(_,_,_,_,_,f,_,_,_,_,_,_,_) -> f) $ s
    , schema . fmap (\(_,_,_,_,_,_,g,_,_,_,_,_,_) -> g) $ s
    , schema . fmap (\(_,_,_,_,_,_,_,h,_,_,_,_,_) -> h) $ s
    , schema . fmap (\(_,_,_,_,_,_,_,_,i,_,_,_,_) -> i) $ s
    , schema . fmap (\(_,_,_,_,_,_,_,_,_,j,_,_,_) -> j) $ s
    , schema . fmap (\(_,_,_,_,_,_,_,_,_,_,k,_,_) -> k) $ s
    , schema . fmap (\(_,_,_,_,_,_,_,_,_,_,_,l,_) -> l) $ s
    , schema . fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,m) -> m) $ s
    ]

instance (JSONSchema a, JSONSchema b, JSONSchema c, JSONSchema d, JSONSchema e, JSONSchema f, JSONSchema g, JSONSchema h, JSONSchema i, JSONSchema j, JSONSchema k, JSONSchema l, JSONSchema m, JSONSchema n) => JSONSchema (a, b, c, d, e, f, g, h, i, j, k, l, m, n) where
  schema s = Tuple
    [ schema . fmap (\(a,_,_,_,_,_,_,_,_,_,_,_,_,_) -> a) $ s
    , schema . fmap (\(_,b,_,_,_,_,_,_,_,_,_,_,_,_) -> b) $ s
    , schema . fmap (\(_,_,c,_,_,_,_,_,_,_,_,_,_,_) -> c) $ s
    , schema . fmap (\(_,_,_,d,_,_,_,_,_,_,_,_,_,_) -> d) $ s
    , schema . fmap (\(_,_,_,_,e,_,_,_,_,_,_,_,_,_) -> e) $ s
    , schema . fmap (\(_,_,_,_,_,f,_,_,_,_,_,_,_,_) -> f) $ s
    , schema . fmap (\(_,_,_,_,_,_,g,_,_,_,_,_,_,_) -> g) $ s
    , schema . fmap (\(_,_,_,_,_,_,_,h,_,_,_,_,_,_) -> h) $ s
    , schema . fmap (\(_,_,_,_,_,_,_,_,i,_,_,_,_,_) -> i) $ s
    , schema . fmap (\(_,_,_,_,_,_,_,_,_,j,_,_,_,_) -> j) $ s
    , schema . fmap (\(_,_,_,_,_,_,_,_,_,_,k,_,_,_) -> k) $ s
    , schema . fmap (\(_,_,_,_,_,_,_,_,_,_,_,l,_,_) -> l) $ s
    , schema . fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,m,_) -> m) $ s
    , schema . fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,n) -> n) $ s
    ]

instance (JSONSchema a, JSONSchema b, JSONSchema c, JSONSchema d, JSONSchema e, JSONSchema f, JSONSchema g, JSONSchema h, JSONSchema i, JSONSchema j, JSONSchema k, JSONSchema l, JSONSchema m, JSONSchema n, JSONSchema o) => JSONSchema (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) where
  schema s = Tuple
    [ schema . fmap (\(a,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> a) $ s
    , schema . fmap (\(_,b,_,_,_,_,_,_,_,_,_,_,_,_,_) -> b) $ s
    , schema . fmap (\(_,_,c,_,_,_,_,_,_,_,_,_,_,_,_) -> c) $ s
    , schema . fmap (\(_,_,_,d,_,_,_,_,_,_,_,_,_,_,_) -> d) $ s
    , schema . fmap (\(_,_,_,_,e,_,_,_,_,_,_,_,_,_,_) -> e) $ s
    , schema . fmap (\(_,_,_,_,_,f,_,_,_,_,_,_,_,_,_) -> f) $ s
    , schema . fmap (\(_,_,_,_,_,_,g,_,_,_,_,_,_,_,_) -> g) $ s
    , schema . fmap (\(_,_,_,_,_,_,_,h,_,_,_,_,_,_,_) -> h) $ s
    , schema . fmap (\(_,_,_,_,_,_,_,_,i,_,_,_,_,_,_) -> i) $ s
    , schema . fmap (\(_,_,_,_,_,_,_,_,_,j,_,_,_,_,_) -> j) $ s
    , schema . fmap (\(_,_,_,_,_,_,_,_,_,_,k,_,_,_,_) -> k) $ s
    , schema . fmap (\(_,_,_,_,_,_,_,_,_,_,_,l,_,_,_) -> l) $ s
    , schema . fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,m,_,_) -> m) $ s
    , schema . fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,n,_) -> n) $ s
    , schema . fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,_,o) -> o) $ s
    ]

-- | Convert a Schema to JSON Schema spec format (json-schema.org draft-07 compliant)
--
-- This function converts the Haskell Schema AST to a JSON value that conforms
-- to the JSON Schema specification, making it suitable for use with tools
-- that expect standard JSON Schema format (e.g., LLMs, validators, documentation generators).
schemaToJSONSchema :: Schema -> Aeson.Value
schemaToJSONSchema = schemaToJSON

-- | ToJSON instance for Schema type
-- Outputs JSON Schema draft-07 compliant format
instance Aeson.ToJSON Schema where
  toJSON = schemaToJSON

-- Internal conversion function
schemaToJSON :: Schema -> Aeson.Value
schemaToJSON (Boolean) =
  Aeson.object ["type" Aeson..= ("boolean" :: Text)]

schemaToJSON (Number bound) =
  let typeField = ["type" Aeson..= ("number" :: Text)]
      minField = case lower bound of
        Nothing -> []
        Just n  -> ["minimum" Aeson..= n]
      maxField = case upper bound of
        Nothing -> []
        Just n  -> ["maximum" Aeson..= n]
  in Aeson.object (typeField ++ minField ++ maxField)

schemaToJSON (Value lengthBound) =
  let typeField = ["type" Aeson..= ("string" :: Text)]
      minField = case lowerLength lengthBound of
        Nothing -> []
        Just n  -> ["minLength" Aeson..= n]
      maxField = case upperLength lengthBound of
        Nothing -> []
        Just n  -> ["maxLength" Aeson..= n]
  in Aeson.object (typeField ++ minField ++ maxField)

schemaToJSON (Object fields) =
  let typeField = ["type" Aeson..= ("object" :: Text)]
      requiredFields = [k | Field k True _ <- fields]
      requiredField = if null requiredFields
                        then []
                        else ["required" Aeson..= requiredFields]
      propertiesObj = mkObject [(k, schemaToJSON s) | Field k _ s <- fields]
      propertiesField = ["properties" Aeson..= propertiesObj]
  in Aeson.object (typeField ++ requiredField ++ propertiesField)

schemaToJSON (Map itemSchema) =
  Aeson.object
    [ "type" Aeson..= ("object" :: Text)
    , "additionalProperties" Aeson..= schemaToJSON itemSchema
    ]

schemaToJSON (Array lengthBound uniqueItems itemSchema) =
  let typeField = ["type" Aeson..= ("array" :: Text)]
      itemsField = ["items" Aeson..= schemaToJSON itemSchema]
      minField = case lowerLength lengthBound of
        Nothing -> []
        Just n  -> ["minItems" Aeson..= n]
      maxField = case upperLength lengthBound of
        Nothing -> []
        Just n  -> ["maxItems" Aeson..= n]
      uniqueField = if uniqueItems
                      then ["uniqueItems" Aeson..= True]
                      else []
  in Aeson.object (typeField ++ itemsField ++ minField ++ maxField ++ uniqueField)

schemaToJSON (Tuple schemas) =
  Aeson.object
    [ "type" Aeson..= ("array" :: Text)
    , "items" Aeson..= map schemaToJSON schemas
    , "minItems" Aeson..= length schemas
    , "maxItems" Aeson..= length schemas
    ]

schemaToJSON (Choice schemas) =
  Aeson.object ["anyOf" Aeson..= map schemaToJSON schemas]

schemaToJSON (Constant val) =
  Aeson.object ["const" Aeson..= val]

schemaToJSON Any =
  Aeson.object []

-- Helper function to create an object from key-value pairs
-- Uses KeyMap for aeson 2.x, HashMap for older versions
mkObject :: [(Text, Aeson.Value)] -> Aeson.Value
#if MIN_VERSION_aeson(2,0,0)
mkObject = Aeson.Object . KeyMap.fromList . map (\(k, v) -> (Key.fromText k, v))
#else
mkObject = Aeson.Object . H.fromList
#endif
