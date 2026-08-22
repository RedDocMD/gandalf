{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE TemplateHaskell       #-}

-- | Parses a sky130-style PDK layer-map JSON (see layers/sky130_layers.json).
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

-- | One entry of "layers": the GDS (layer, datatype) pair behind a named
-- mask/purpose, e.g. "li1.drawing" -> layer 67, datatype 20.
data LayerEntry = LayerEntry
  { name     :: String
  , layer    :: Int
  , datatype :: Int
  } deriving (Show)

-- | One entry of "direct_connections": a named layer devices connect to
-- directly, without a via.
newtype DirectConnection = DirectConnection
  { layer :: String
  } deriving (Show)

-- | One entry of "cross_connections": a named via layer connecting a pair
-- of named layers.
data CrossConnection = CrossConnection
  { layers :: [String]
  , via    :: String
  } deriving (Show)

data LayerMap = LayerMap
  { layers            :: [LayerEntry]
  , directConnections :: [DirectConnection]
  , crossConnections  :: [CrossConnection]
  } deriving (Show)

-- A single snake_case fieldLabelModifier covers every field here
-- (directConnections -> direct_connections, etc). Inlined at each splice
-- rather than bound to a top-level name: GHC's stage restriction forbids a
-- top-level splice from using an ordinary binding from the same module.
$(deriveFromJSON defaultOptions { fieldLabelModifier = camelTo2 '_' } ''LayerEntry)
$(deriveFromJSON defaultOptions { fieldLabelModifier = camelTo2 '_' } ''DirectConnection)
$(deriveFromJSON defaultOptions { fieldLabelModifier = camelTo2 '_' } ''CrossConnection)
$(deriveFromJSON defaultOptions { fieldLabelModifier = camelTo2 '_' } ''LayerMap)

-- | Errors with aeson's parse failure message if malformed.
readLayerMap :: FilePath -> IO LayerMap
readLayerMap path = do
  parsed <- eitherDecodeFileStrict path
  case parsed of
    Right lm  -> return lm
    Left  err -> error (toText ("readLayerMap: failed to parse " ++ path ++ ": " ++ err))
