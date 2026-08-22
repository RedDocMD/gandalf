{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

module Structure where
import           AST   (AST (..), astKind, findChild, findChildren)
import           Parse (GdsPresentationFlags, GdsRecord (..), GdsRecordT (..),
                        GdsStransFlags)

data Layer = Layer
  { index :: Int
  , kind  :: Int
  } deriving (Show, Eq, Ord)

data Coordinate = Coordinate
  { x :: Int
  , y :: Int
  } deriving (Show, Eq, Ord)

data Boundary = Boundary
  { layer  :: Layer
  , coords :: [Coordinate]
  } deriving (Show)

data PathKind =
    Flush
  | Round
  | Extended
  | VariableExtended {
    start :: Int
  , end   :: Int
  } deriving (Show)

data Path = Path
  { layer :: Layer
  , width :: Int
  , start :: Coordinate
  , end   :: Coordinate
  , kind  :: PathKind
  } deriving (Show)

data CellRef = CellRef
  { name        :: String
  , coord       :: Coordinate
  , translation :: Maybe GdsStransFlags
  , angle       :: Maybe Double
  } deriving (Show)

data TextDescription = TextDescription
  { layer         :: Layer
  , coord         :: Coordinate
  , value         :: String
  , translation   :: Maybe GdsStransFlags
  , angle         :: Maybe Double
  , presentation  :: Maybe GdsPresentationFlags
  , magnification :: Maybe Double
  } deriving (Show)

data Cell = Cell
  { name     :: String
  , boundary :: [Boundary]
  , path     :: [Path]
  , text     :: [TextDescription]
  , cellRef  :: [CellRef]
  } deriving (Show)

-- Parsing: AST -> Structure

-- | relude's error wants Text; single conversion point for this module.
parseError :: String -> a
parseError = error . toText

-- | The payload of a record expected to hold a single Int; errors otherwise.
intPayload :: GdsRecordT -> Int
intPayload r = case r of
  GdsHeaderT n   -> n
  GdsLayerT n    -> n
  GdsDataTypeT n -> n
  GdsWidthT n    -> n
  GdsTextTypeT n -> n
  GdsPathTypeT n -> n
  GdsBgnExtnT n  -> n
  GdsEndExtnT n  -> n
  _              -> parseError ("expected an Int-valued record, got " ++ show r)

stringPayload :: GdsRecordT -> String
stringPayload r = case r of
  GdsLibNameT s -> s
  GdsStrNameT s -> s
  GdsSnameT s   -> s
  GdsStringT s  -> s
  _             -> parseError ("expected a String-valued record, got " ++ show r)

doublePayload :: GdsRecordT -> Double
doublePayload r = case r of
  GdsMagT d   -> d
  GdsAngleT d -> d
  _           -> parseError ("expected a Double-valued record, got " ++ show r)

stransPayload :: GdsRecordT -> GdsStransFlags
stransPayload (GdsStransT flags) = flags
stransPayload r                  = parseError ("expected a Strans record, got " ++ show r)

presentationPayload :: GdsRecordT -> GdsPresentationFlags
presentationPayload (GdsPresentationT flags) = flags
presentationPayload r                        = parseError ("expected a Presentation record, got " ++ show r)

-- | Looks up a required child, erroring with the given context (e.g. the
-- enclosing record kind) if absent.
requireChild :: String -> GdsRecord -> [AST] -> AST
requireChild ctx wantedKind children = case findChild wantedKind children of
  Just child -> child
  Nothing    -> parseError (ctx ++ ": missing required " ++ show wantedKind)

-- | 'last' for lists known non-empty by construction (relude hides Prelude's).
lastOf :: [a] -> a
lastOf [v]      = v
lastOf (_ : xs) = lastOf xs
lastOf []       = parseError "lastOf: empty list"

-- | Builds a Layer from the mandatory GdsLayer child plus a caller-chosen
-- kind record (GdsDataType for Boundary/Path, GdsTextType for Text).
parseLayer :: GdsRecord -> [AST] -> Layer
parseLayer kindRecord children = Layer
  { index = intPayload (astRecord (requireChild "Layer" GdsLayer children))
  , kind  = intPayload (astRecord (requireChild "Layer" kindRecord children))
  }

parseCoordinate :: (Int32, Int32) -> Coordinate
parseCoordinate (px, py) = Coordinate (fromIntegral px) (fromIntegral py)

parseCoordinates :: [AST] -> [Coordinate]
parseCoordinates children = case astRecord (requireChild "Xy" GdsXy children) of
  GdsXyT pts -> map parseCoordinate pts
  r          -> parseError ("expected an Xy record, got " ++ show r)

parsePathKind :: [AST] -> PathKind
parsePathKind children = case intPayload (astRecord (requireChild "Path" GdsPathType children)) of
  0 -> Flush
  1 -> Round
  2 -> Extended
  4 -> VariableExtended
    { start = intPayload (astRecord (requireChild "Path" GdsBgnExtn children))
    , end   = intPayload (astRecord (requireChild "Path" GdsEndExtn children))
    }
  n -> parseError ("parsePathKind: unknown path type " ++ show n)

parseBoundary :: AST -> Boundary
parseBoundary node
  | astKind node /= GdsBoundary = parseError "parseBoundary: expected a Boundary node"
  | otherwise = Boundary
      { layer  = parseLayer GdsDataType children
      , coords = parseCoordinates children
      }
  where children = astChildren node

parsePath :: AST -> Path
parsePath node
  | astKind node /= GdsPath = parseError "parsePath: expected a Path node"
  | otherwise = case parseCoordinates children of
      (s : rest@(_ : _)) -> Path
        { layer = parseLayer GdsDataType children
        , width = intPayload (astRecord (requireChild "Path" GdsWidth children))
        , start = s
        , end   = lastOf rest
        , kind  = parsePathKind children
        }
      _ -> parseError "parsePath: Xy must contain at least two points"
  where children = astChildren node

parseCellRef :: AST -> CellRef
parseCellRef node
  | astKind node /= GdsSref = parseError "parseCellRef: expected a Sref node"
  | otherwise = case parseCoordinates children of
      (c : _) -> CellRef
        { name        = stringPayload (astRecord (requireChild "Sref" GdsSname children))
        , coord       = c
        , translation = stransPayload . astRecord <$> findChild GdsStrans children
        , angle       = doublePayload . astRecord <$> findChild GdsAngle children
        }
      [] -> parseError "parseCellRef: Xy has no points"
  where children = astChildren node

parseTextDescription :: AST -> TextDescription
parseTextDescription node
  | astKind node /= GdsText = parseError "parseTextDescription: expected a Text node"
  | otherwise = case parseCoordinates children of
      (c : _) -> TextDescription
        { layer         = parseLayer GdsTextType children
        , coord         = c
        , value         = stringPayload (astRecord (requireChild "Text" GdsString children))
        , translation   = stransPayload . astRecord <$> findChild GdsStrans children
        , angle         = doublePayload . astRecord <$> findChild GdsAngle children
        , presentation  = presentationPayload . astRecord <$> findChild GdsPresentation children
        , magnification = doublePayload . astRecord <$> findChild GdsMag children
        }
      [] -> parseError "parseTextDescription: Xy has no points"
  where children = astChildren node

parseCell :: AST -> Cell
parseCell node
  | astKind node /= GdsBgnStr = parseError "parseCell: expected a BgnStr node"
  | otherwise = Cell
      { name     = stringPayload (astRecord (requireChild "Cell" GdsStrName children))
      , boundary = map parseBoundary (findChildren GdsBoundary children)
      , path     = map parsePath (findChildren GdsPath children)
      , text     = map parseTextDescription (findChildren GdsText children)
      , cellRef  = map parseCellRef (findChildren GdsSref children)
      }
  where children = astChildren node

-- | Parses every BGNSTR structure in a library's AST forest into a Cell.
parseCells :: [AST] -> [Cell]
parseCells forest = map parseCell (findChildren GdsBgnStr children)
  where children = astChildren (requireChild "Library" GdsBgnLib forest)
