module Main where

import           AST                        (AST (..), astKind, buildForest,
                                              isCloser, isOpener)
import qualified Data.Attoparsec.ByteString as DAP
import qualified Data.ByteString            as BS
import           Data.List                  (intercalate)
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (catMaybes)
import qualified Data.Set                   as Set
import           Options.Applicative
import           Parse                      (GdsPresentationFlags (GdsPresentationFlags),
                                              GdsRecord, GdsRecordT,
                                              GdsStransFlags (GdsStransFlags),
                                              parseGdsRecord)
import           Structure                  (Boundary (Boundary),
                                              Cell (Cell), CellRef (CellRef),
                                              Coordinate (Coordinate),
                                              Path (Path),
                                              TextDescription (TextDescription),
                                              parseCells)

data Command = Dump FilePath (Maybe String) | Elstr FilePath

commandParser :: Parser Command
commandParser = subparser
  ( command "dump" (info (dumpParser <**> helper) (progDesc "Parse a GDS file and print its cells"))
 <> command "elstr" (info (elstrParser <**> helper) (progDesc "Summarize the unique sets of sub-units found within each hierarchical GDS unit")) )

dumpParser :: Parser Command
dumpParser = Dump <$> fileArgument <*> cellNameOption

cellNameOption :: Parser (Maybe String)
cellNameOption = optional . strOption $
     long "cell"
  <> short 'c'
  <> metavar "NAME"
  <> help "Only dump the cell with this name"

elstrParser :: Parser Command
elstrParser = Elstr <$> fileArgument

fileArgument :: Parser FilePath
fileArgument = argument str (metavar "FILE" <> help "Path to a GDS file")

main :: IO ()
main = do
  cmd <- execParser opts
  case cmd of
    Dump path cellName -> runDump path cellName
    Elstr path         -> runElstr path
  where
    opts = info (commandParser <**> helper)
      ( fullDesc <> progDesc "Gandalf: parse and inspect GDSII files" )

runDump :: FilePath -> Maybe String -> IO ()
runDump path cellName = do
  contents <- BS.readFile path
  let cells    = parseCells (buildForest (parseAllRecords contents))
      selected = maybe cells (\nm -> filter (\(Cell cnm _ _ _ _) -> cnm == nm) cells) cellName
  mapM_ putStrLn (renderForest (map cellTree selected))

-- | A label together with the labels nested beneath it - used to render
-- parsed Cells with the same box-drawing style as the raw record dump used
-- to, rather than Structure's own (flat) Show instances.
data Tree = Tree String [Tree]

cellTree :: Cell -> Tree
cellTree (Cell nm bnds paths texts refs) =
  Tree ("Cell " ++ show nm)
    (map boundaryTree bnds ++ map pathTree paths ++ map textTree texts ++ map cellRefTree refs)

boundaryTree :: Boundary -> Tree
boundaryTree (Boundary lyr crds) =
  Tree "Boundary"
    [ leaf "layer" (show lyr)
    , leaf "coords" (showCoordinates crds)
    ]

pathTree :: Path -> Tree
pathTree (Path lyr w s e k) =
  Tree "Path"
    [ leaf "layer" (show lyr)
    , leaf "width" (show w)
    , leaf "start" (showCoordinate s)
    , leaf "end" (showCoordinate e)
    , leaf "kind" (show k)
    ]

cellRefTree :: CellRef -> Tree
cellRefTree (CellRef nm crd trans ang) =
  Tree "Sref" $
    [ leaf "name" (show nm)
    , leaf "coord" (showCoordinate crd)
    ] ++ catMaybes
    [ leaf "translation" . showStrans <$> trans
    , leaf "angle" . show <$> ang
    ]

textTree :: TextDescription -> Tree
textTree (TextDescription lyr crd val trans ang pres mag) =
  Tree "Text" $
    [ leaf "layer" (show lyr)
    , leaf "coord" (showCoordinate crd)
    , leaf "value" (show val)
    ] ++ catMaybes
    [ leaf "translation" . showStrans <$> trans
    , leaf "angle" . show <$> ang
    , leaf "presentation" . showPresentation <$> pres
    , leaf "magnification" . show <$> mag
    ]

leaf :: String -> String -> Tree
leaf label s = Tree (label ++ ": " ++ s) []

showCoordinate :: Coordinate -> String
showCoordinate (Coordinate cx cy) = "(" ++ show cx ++ ", " ++ show cy ++ ")"

showCoordinates :: [Coordinate] -> String
showCoordinates cs = "[" ++ intercalate ", " (map showCoordinate cs) ++ "]"

-- | A terser stand-in for GdsStransFlags's derived Show, dropping the type
-- name and record-field boilerplate.
showStrans :: GdsStransFlags -> String
showStrans (GdsStransFlags mx am aa) =
  "{mirrorX=" ++ show mx ++ ", absMag=" ++ show am ++ ", absAngle=" ++ show aa ++ "}"

showPresentation :: GdsPresentationFlags -> String
showPresentation (GdsPresentationFlags f vj hj) =
  "{font=" ++ show f ++ ", vertJust=" ++ show vj ++ ", horizJust=" ++ show hj ++ "}"

-- | Renders a forest using the same box-drawing style as the Unix `tree`
-- command: "├── " / "└── " connectors and "│   " / "    " continuations.
renderForest :: [Tree] -> [String]
renderForest ts = concatMap (\(t, isLast) -> renderTree "" isLast t) (markLast ts)

renderTree :: String -> Bool -> Tree -> [String]
renderTree prefix isLast (Tree label children) =
  (prefix ++ connector ++ label) :
  concatMap (\(c, isLastChild) -> renderTree childPrefix isLastChild c) (markLast children)
  where
    connector   = if isLast then "└── " else "├── "
    childPrefix = prefix ++ if isLast then "    " else "│   "

markLast :: [a] -> [(a, Bool)]
markLast []       = []
markLast [x]      = [(x, True)]
markLast (x : xs) = (x, False) : markLast xs

runElstr :: FilePath -> IO ()
runElstr path = do
  contents <- BS.readFile path
  let forest = buildForest (parseAllRecords contents)
  mapM_ putStrLn (renderElstr (collectUnitShapes forest))

-- | For each hierarchical unit kind (BGNSTR, BOUNDARY, ...), the distinct
-- sets of immediate sub-unit kinds seen across all its instances in the
-- file, each paired with a per-element count: for every sub-unit kind in
-- the set, the largest number of times it occurred within a single
-- instance, among all instances sharing that exact set (e.g. if one
-- BGNSTR has three BOUNDARY children and another sharing the same
-- {STRNAME, BOUNDARY} set has only one, the recorded max for BOUNDARY is
-- 3). The closing record (ENDEL/ENDSTR/ENDLIB) is always present and so
-- isn't interesting; it's excluded from the shape and the counts.
collectUnitShapes :: [AST] -> Map.Map GdsRecord (Map.Map (Set.Set GdsRecord) (Map.Map GdsRecord Int))
collectUnitShapes forest = Map.fromListWith (Map.unionWith (Map.unionWith max))
  [ (kind, Map.singleton shape counts) | (kind, shape, counts) <- unitShapes forest ]

unitShapes :: [AST] -> [(GdsRecord, Set.Set GdsRecord, Map.Map GdsRecord Int)]
unitShapes = concatMap go
  where
    go node@(AST r children) =
      [ (astKind node, shape, counts) | isOpener r ] ++ unitShapes children
      where
        childKinds = [ astKind c | c@(AST cr _) <- children, not (isCloser cr) ]
        shape      = Set.fromList childKinds
        counts     = Map.fromListWith (+) [ (k, 1 :: Int) | k <- childKinds ]

renderElstr :: Map.Map GdsRecord (Map.Map (Set.Set GdsRecord) (Map.Map GdsRecord Int)) -> [String]
renderElstr = concatMap renderUnit . Map.toAscList
  where
    renderUnit (kind, shapes) =
      show kind : map (("  " ++) . renderShape) (Map.toAscList shapes)
    renderShape (shape, counts) =
      "{" ++ intercalate ", " (map (renderElem counts) (Set.toAscList shape)) ++ "}"
    renderElem counts k = show k ++ " (max: " ++ show (Map.findWithDefault 0 k counts) ++ ")"

parseAllRecords :: BS.ByteString -> [GdsRecordT]
parseAllRecords = go
  where
    go bs
      | BS.null bs = []
      | otherwise = case DAP.parse parseGdsRecord bs of
          DAP.Done rest r    -> r : go rest
          DAP.Fail _ _ msg   -> error ("failed to parse GDS record: " ++ msg)
          DAP.Partial cont   -> case cont BS.empty of
            DAP.Done rest r  -> r : go rest
            DAP.Fail _ _ msg -> error ("failed to parse GDS record: " ++ msg)
            DAP.Partial _    -> error "parser requested more input after EOF"
