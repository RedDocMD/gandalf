{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE TemplateHaskell       #-}

-- | Parses a component-list YAML file: a top-level mapping from component
-- name to its type and pins, e.g.
--
-- > inv1:
-- >   type: inverter
-- >   pins:
-- >     - name: A
-- >       layer: li1
-- >     - name: Y
-- >       layer: li1
module Component
  ( Pin (..)
  , Component (..)
  , ComponentList
  , readComponentList
  ) where

import           Data.Aeson.TH   (defaultOptions, deriveFromJSON,
                                   fieldLabelModifier)
import qualified Data.Map.Strict as Map
import           Data.Yaml       (decodeFileEither, prettyPrintParseException)

-- | One entry of a component's "pins" list.
data Pin = Pin
  { name  :: String
  , layer :: String
  } deriving (Show, Eq)

-- | A single component: its type plus its pins. The field is named
-- 'componentType' rather than 'type' since the latter is a Haskell
-- keyword; the fieldLabelModifier below maps it back to the YAML key
-- "type".
data Component = Component
  { componentType :: String
  , pins          :: [Pin]
  } deriving (Show, Eq)

-- | The full contents of a component-list YAML file: component name ->
-- Component. The top-level mapping has no wrapper key, so this is a plain
-- type alias over aeson's built-in Map FromJSON instance rather than a
-- record needing its own TH-derived instance.
type ComponentList = Map.Map String Component

$(deriveFromJSON defaultOptions ''Pin)
$(deriveFromJSON defaultOptions
    { fieldLabelModifier = \f -> if f == "componentType" then "type" else f
    } ''Component)

-- | Reads and parses a component-list YAML file, erroring out with yaml's
-- parse failure message if it's malformed.
readComponentList :: FilePath -> IO ComponentList
readComponentList path = do
  parsed <- decodeFileEither path
  case parsed of
    Right cl  -> return cl
    Left  err -> error (toText ("readComponentList: failed to parse " ++ path ++ ": " ++ prettyPrintParseException err))
