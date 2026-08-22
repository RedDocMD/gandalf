{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE TemplateHaskell       #-}

-- | Parses a component-list YAML file (component name -> type and pins), e.g.
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

data Pin = Pin
  { name  :: String
  , layer :: String
  } deriving (Show, Eq)

-- | 'componentType' rather than 'type' (a Haskell keyword); the
-- fieldLabelModifier below maps it back to the YAML key "type".
data Component = Component
  { componentType :: String
  , pins          :: [Pin]
  } deriving (Show, Eq)

type ComponentList = Map.Map String Component

$(deriveFromJSON defaultOptions ''Pin)
$(deriveFromJSON defaultOptions
    { fieldLabelModifier = \f -> if f == "componentType" then "type" else f
    } ''Component)

-- | Errors with yaml's parse failure message if malformed.
readComponentList :: FilePath -> IO ComponentList
readComponentList path = do
  parsed <- decodeFileEither path
  case parsed of
    Right cl  -> return cl
    Left  err -> error (toText ("readComponentList: failed to parse " ++ path ++ ": " ++ prettyPrintParseException err))
