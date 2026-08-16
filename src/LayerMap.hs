{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE TemplateHaskell       #-}

-- | Parses a sky130-style PDK layer-map JSON (see
-- layers/sky130_layers.json): the GDS (layer, datatype) pair behind every
-- named mask/purpose, plus which named layers directly connect to devices
-- and which pairs of named layers a via layer connects.
module LayerMap
  ( LayerEntry (..)
  , DirectConnection (..)
  , CrossConnection (..)
  , LayerMap (..)
  , readLayerMap
  ) where

import           Data.Aeson       (eitherDecodeFileStrict)
import           Data.Aeson.TH    (defaultOptions, deriveFromJSON,
                                    fieldLabelModifier)
import           Data.Aeson.Types (camelTo2)

-- | One entry of the JSON's "layers" array: the GDS (layer, datatype) pair
-- behind a single named mask/purpose, e.g. "li1.drawing" -> layer 67,
-- datatype 20.
data LayerEntry = LayerEntry
  { name     :: String
  , layer    :: Int
  , datatype :: Int
  } deriving (Show)

-- | One entry of the JSON's "direct_connections" array: a named layer that
-- devices connect to directly, without going through a via.
newtype DirectConnection = DirectConnection
  { layer :: String
  } deriving (Show)

-- | One entry of the JSON's "cross_connections" array: a named via layer
-- that connects a pair of named layers.
data CrossConnection = CrossConnection
  { layers :: [String]
  , via    :: String
  } deriving (Show)

-- | The full contents of a sky130-style layer-map JSON file.
data LayerMap = LayerMap
  { layers            :: [LayerEntry]
  , directConnections :: [DirectConnection]
  , crossConnections  :: [CrossConnection]
  } deriving (Show)

-- name/layer/datatype/via and the elements of "layers" already match their
-- JSON keys verbatim, so a single snake_case field-label modifier is
-- enough to also cover directConnections/crossConnections ->
-- direct_connections/cross_connections without repeating field-by-field
-- renames for each type. (Inlined at each splice, rather than bound to a
-- top-level name, because GHC's stage restriction forbids a top-level
-- splice from using an ordinary binding from the same module.)
$(deriveFromJSON defaultOptions { fieldLabelModifier = camelTo2 '_' } ''LayerEntry)
$(deriveFromJSON defaultOptions { fieldLabelModifier = camelTo2 '_' } ''DirectConnection)
$(deriveFromJSON defaultOptions { fieldLabelModifier = camelTo2 '_' } ''CrossConnection)
$(deriveFromJSON defaultOptions { fieldLabelModifier = camelTo2 '_' } ''LayerMap)

-- | Reads and parses a sky130-style layer-map JSON file, erroring out with
-- aeson's parse failure message if it's malformed.
readLayerMap :: FilePath -> IO LayerMap
readLayerMap path = do
  parsed <- eitherDecodeFileStrict path
  case parsed of
    Right lm  -> return lm
    Left  err -> error (toText ("readLayerMap: failed to parse " ++ path ++ ": " ++ err))
