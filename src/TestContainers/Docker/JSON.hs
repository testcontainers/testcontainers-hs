{-# LANGUAGE OverloadedStrings #-}

-- | Helpers for navigating aeson 'Value' trees without
--   an optics dependency.
module TestContainers.Docker.JSON
  ( lookupKey,
    asText,
    asBool,
    asInteger,
    eachMember,
    eachValue,
  )
where

import Data.Aeson (Value (..), withObject)
import Data.Aeson.Types (parseMaybe, (.:))
import Data.Scientific (floatingOrInteger)
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Vector as Vector

-- | Look up a key in a JSON object. Works across aeson 1.x and 2.x
--   because '(.:)' accepts both 'Text' and 'Key' via 'IsString'.
lookupKey :: String -> Value -> Maybe Value
lookupKey k = parseMaybe (withObject "object" (\obj -> obj .: fromString k))

-- | Extract a 'Text' from a JSON 'String' value.
asText :: Value -> Maybe Text
asText (String t) = Just t
asText _ = Nothing

-- | Extract a 'Bool' from a JSON boolean value.
asBool :: Value -> Maybe Bool
asBool (Bool b) = Just b
asBool _ = Nothing

-- | Extract an 'Integer' from a JSON number value.
asInteger :: Value -> Maybe Integer
asInteger (Number n) =
  case floatingOrInteger n of
    Right i -> Just i
    Left _floatingPoint -> Nothing
asInteger _ = Nothing

-- | Collect all member values from a JSON object.
eachMember :: Value -> [Value]
eachMember (Object obj) = foldMap (: []) obj
eachMember _ = []

-- | Collect all elements from a JSON array.
eachValue :: Value -> [Value]
eachValue (Array arr) = Vector.toList arr
eachValue _ = []
