module Main where

import           AST                        (AST (..), astKind, buildForest,
                                              isCloser, isOpener)
import           Component                  (readComponentList)
import qualified Data.Attoparsec.ByteString as DAP
import qualified Data.ByteString            as BS
import           Data.List                  (find, foldl', intercalate,
                                              transpose)
import qualified Data.Map.Lazy              as MapLazy
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (catMaybes, listToMaybe)
import qualified Data.Set                   as Set
import           Geom                       (Polygon, boundIntersections,
                                              polygonIntersection,
                                              polygonVertices)
import           LayerMap                   (LayerEntry (LayerEntry),
                                              LayerMap (LayerMap),
                                              readLayerMap)
import           Netlist                    (netlistToVerilog, readNetlist)
import           Options.Applicative
import           Parse                      (GdsPresentationFlags (GdsPresentationFlags),
                                              GdsRecord, GdsRecordT,
                                              GdsStransFlags (GdsStransFlags),
                                              parseGdsRecord)
import           Relationship               (LabeledPolygon (LabeledPolygon),
                                              LayerPolygon (LayerPolygon),
                                              Pin (Pin), connectedComponents,
                                              connectivity, layerPolygons,
                                              netlist, pins, pinsByInstance)
import           Structure                  (Boundary (Boundary),
                                              Cell (Cell), CellRef (CellRef),
                                              Coordinate (Coordinate),
                                              Layer (Layer), Path (Path),
                                              TextDescription (TextDescription),
                                              parseCells)
import           Text.Printf                (printf)

data Command
  = Dump FilePath (Maybe String)
  | Elstr FilePath
  | Polycount FilePath String Int Int
  | Intersections FilePath String Int Int Int Int (Maybe FilePath)
  | Connectivity FilePath String FilePath
  | ConnectedComponents FilePath String FilePath
  | PinDump FilePath String FilePath Bool
  | Netlist FilePath String FilePath FilePath String
  | Verilog FilePath FilePath FilePath
  | Labels FilePath

commandParser :: Parser Command
commandParser = subparser
  ( command "dump" (info (dumpParser <**> helper) (progDesc "Parse a GDS file and print its cells"))
 <> command "elstr" (info (elstrParser <**> helper) (progDesc "Summarize the unique sets of sub-units found within each hierarchical GDS unit"))
 <> command "polycount" (info (polycountParser <**> helper) (progDesc "Count boundary and path elements on a given layer/kind within a cell, including elements pulled in via SREF"))
 <> command "intersections" (info (intersectionsParser <**> helper) (progDesc "Count bounding-rectangle and true polygon intersections among the boundary and path elements on two layer/kind pairs in a cell, including elements pulled in via SREF"))
 <> command "connectivity" (info (connectivityParser <**> helper) (progDesc "Summarize direct- and cross-layer physical connectivity within a cell, per a layer-map JSON file, including elements pulled in via SREF"))
 <> command "components" (info (componentsParser <**> helper) (progDesc "Print the total number of connected components (nets) found via direct/cross-layer connectivity within a cell, per a layer-map JSON file, including elements pulled in via SREF"))
 <> command "pins" (info (pinsParser <**> helper) (progDesc "Dump every pin found within a cell, per a layer-map JSON file, grouped by the specific instance (SREF placement) it belongs to"))
 <> command "netlist" (info (netlistParser <**> helper) (progDesc "Trace a netlist of <component.pin> -- <component.pin> connections reachable from one output pin on a cell, per a layer-map JSON file and a components YAML file naming which SREF instances are components (see layers/sky130_layers.json, pins/puzzle.yaml)"))
 <> command "verilog" (info (verilogParser <**> helper) (progDesc "Convert an extracted netlist text file into a structural Verilog module, per a components YAML file naming every placed cell type's pins (see pins/demo.yaml, netlist/demo.netlist)"))
 <> command "labels" (info (labelsParser <**> helper) (progDesc "Print every string block's label found in each cell of a GDS file, without expanding elements pulled in via SREF")) )

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

polycountParser :: Parser Command
polycountParser = Polycount
  <$> fileArgument
  <*> argument str (metavar "CELL" <> help "Name of the cell to count elements in")
  <*> argument auto (metavar "LAYER" <> help "Layer index to match")
  <*> argument auto (metavar "KIND" <> help "Layer kind (datatype) to match")

intersectionsParser :: Parser Command
intersectionsParser = Intersections
  <$> fileArgument
  <*> argument str (metavar "CELL" <> help "Name of the cell to find intersections in")
  <*> argument auto (metavar "LAYER1" <> help "First layer index")
  <*> argument auto (metavar "KIND1" <> help "First layer kind (datatype)")
  <*> argument auto (metavar "LAYER2" <> help "Second layer index")
  <*> argument auto (metavar "KIND2" <> help "Second layer kind (datatype)")
  <*> optional (strOption
        (  long "output"
        <> short 'o'
        <> metavar "SVG_FILE"
        <> help "Write an SVG diagram of both layers' shapes and their true intersections to this file"
        ))

connectivityParser :: Parser Command
connectivityParser = Connectivity
  <$> fileArgument
  <*> argument str (metavar "CELL" <> help "Name of the cell to find connectivity in")
  <*> argument str (metavar "LAYER_MAP" <> help "Path to a layer-map JSON file describing named layers and their direct/cross connections (see layers/sky130_layers.json)")

componentsParser :: Parser Command
componentsParser = ConnectedComponents
  <$> fileArgument
  <*> argument str (metavar "CELL" <> help "Name of the cell to find connected components in")
  <*> argument str (metavar "LAYER_MAP" <> help "Path to a layer-map JSON file describing named layers and their direct/cross connections (see layers/sky130_layers.json)")

pinsParser :: Parser Command
pinsParser = PinDump
  <$> fileArgument
  <*> argument str (metavar "CELL" <> help "Name of the cell to dump pins for")
  <*> argument str (metavar "LAYER_MAP" <> help "Path to a layer-map JSON file describing named '.pin' layers (see layers/sky130_layers.json)")
  <*> switch
        (  long "merge-instances"
        <> short 'm'
        <> help "Treat every instantiation of a Cell (direct or via SREF) as the same, printing each Cell's pins once instead of once per instance, without a Polygon boundary (which would differ, meaninglessly, per instance)"
        )

netlistParser :: Parser Command
netlistParser = Netlist
  <$> fileArgument
  <*> argument str (metavar "CELL" <> help "Name of the top-level cell to trace a netlist from")
  <*> argument str (metavar "LAYER_MAP" <> help "Path to a layer-map JSON file describing named layers and their direct/cross connections (see layers/sky130_layers.json)")
  <*> argument str (metavar "COMPONENTS" <> help "Path to a components YAML file naming every placed Cell type's expected pins (see pins/puzzle.yaml)")
  <*> argument str (metavar "PIN" <> help "Name of an output pin directly on the top-level cell to trace the netlist from")

verilogParser :: Parser Command
verilogParser = Verilog
  <$> argument str (metavar "COMPONENTS" <> help "Path to a components YAML file naming every placed cell type's pins (see pins/demo.yaml)")
  <*> argument str (metavar "NETLIST" <> help "Path to an extracted netlist text file (see netlist/demo.netlist)")
  <*> argument str (metavar "OUTPUT" <> help "Path to write the generated Verilog module to")

labelsParser :: Parser Command
labelsParser = Labels <$> fileArgument

fileArgument :: Parser FilePath
fileArgument = argument str (metavar "FILE" <> help "Path to a GDS file")

main :: IO ()
main = do
  cmd <- execParser opts
  case cmd of
    Dump path cellName                      -> runDump path cellName
    Elstr path                              -> runElstr path
    Polycount path cellName l k             -> runPolycount path cellName l k
    Intersections path cellName l1 k1 l2 k2 out -> runIntersections path cellName l1 k1 l2 k2 out
    Connectivity path cellName layerMapPath -> runConnectivity path cellName layerMapPath
    ConnectedComponents path cellName layerMapPath -> runConnectedComponents path cellName layerMapPath
    PinDump path cellName layerMapPath merge -> runPinDump path cellName layerMapPath merge
    Netlist path cellName layerMapPath componentsPath outputPin ->
      runNetlist path cellName layerMapPath componentsPath outputPin
    Verilog componentsPath netlistPath outputPath ->
      runVerilog componentsPath netlistPath outputPath
    Labels path -> runLabels path
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

runPolycount :: FilePath -> String -> Int -> Int -> IO ()
runPolycount path cellName layerIdx layerKind = do
  contents <- BS.readFile path
  let cells  = parseCells (buildForest (parseAllRecords contents))
      counts = polycounts (matchesLayer layerIdx layerKind) cells
  case Map.lookup cellName counts of
    Just n  -> print n
    Nothing -> error ("polycount: no such cell " ++ show cellName)

-- | Total boundary + path element count on a given layer for every cell,
-- including elements pulled in transitively through SREFs. Built by
-- knot-tying: each cell's count looks up the memoized counts of the
-- cells it references from the very map being constructed, so laziness
-- resolves the recursion without an explicit topological sort or
-- traversal order (this would only loop if the library had a cyclic
-- SREF chain, which GDS doesn't permit).
--
-- This must be built with Data.Map.Lazy, not .Strict: Strict's fromList
-- forces each value into WHNF as it inserts, which demands the
-- not-yet-finished map's spine before the knot can close and throws
-- <<loop>> even for a plain DAG of references.
polycounts :: (Layer -> Bool) -> [Cell] -> Map.Map String Int
polycounts matches cells = counts
  where
    counts = MapLazy.fromList [ (nm, cellCount c) | c@(Cell nm _ _ _ _) <- cells ]
    cellCount (Cell _ bnds paths _ refs) =
      length (filter (\(Boundary lyr _) -> matches lyr) bnds)
      + length (filter (\(Path lyr _ _ _ _) -> matches lyr) paths)
      + sum [ MapLazy.findWithDefault 0 refNm counts | CellRef refNm _ _ _ <- refs ]

matchesLayer :: Int -> Int -> Layer -> Bool
matchesLayer wantIdx wantKind (Layer li lk) = li == wantIdx && lk == wantKind

runIntersections :: FilePath -> String -> Int -> Int -> Int -> Int -> Maybe FilePath -> IO ()
runIntersections path cellName l1Idx l1Kind l2Idx l2Kind outputSvg = do
  contents <- BS.readFile path
  let cells = parseCells (buildForest (parseAllRecords contents))
  case find (\(Cell nm _ _ _ _) -> nm == cellName) cells of
    Just root -> do
      let grouped  = layerPolygons cells root
          ps1      = layerPolygonsOn grouped l1Idx l1Kind
          ps2      = layerPolygonsOn grouped l2Idx l2Kind
          boxPairs = boundIntersections (ps1 ++ ps2)
          -- boundIntersections only guarantees the pairs' bounding rectangles
          -- overlap; polygonIntersection narrows that down to the pairs (and
          -- exact regions) that truly overlap. Each pair is only intersected
          -- once and reused for both the count and the diagram below.
          truePairs         = catMaybes [ polygonIntersection p1 p2 | (p1, p2) <- boxPairs ]
          intersectionPolys = concat truePairs
      putStr $ renderIntersectionSummary
        [ ("Layer 1 shapes", length ps1)
        , ("Layer 2 shapes", length ps2)
        , ("Bounding-box intersections", length boxPairs)
        , ("True intersections", length truePairs)
        ]
      case outputSvg of
        Nothing      -> return ()
        Just svgPath -> do
          writeFile svgPath (renderSvg cellName ps1 ps2 intersectionPolys)
          putStrLn ("SVG diagram written to " ++ svgPath)
    Nothing -> error ("intersections: no such cell " ++ show cellName)

-- | The plain Polygons - parent labels dropped - on a given layer/kind of a
-- Relationship.layerPolygons grouping.
layerPolygonsOn :: Map.Map Layer [LabeledPolygon] -> Int -> Int -> [Polygon]
layerPolygonsOn grouped idx kind =
  [ p | LabeledPolygon p _ <- Map.findWithDefault [] (Layer idx kind) grouped ]

-- | Renders label/count rows as a small aligned table, e.g.:
--
-- > Layer 1 shapes             : 12
-- > Bounding-box intersections : 5
renderIntersectionSummary :: [(String, Int)] -> String
renderIntersectionSummary rows = unlines (map renderRow rows)
  where
    labelWidth = maximum (map (length . fst) rows)
    renderRow (label, n) =
      label ++ replicate (labelWidth - length label) ' ' ++ " : " ++ show n

runConnectivity :: FilePath -> String -> FilePath -> IO ()
runConnectivity path cellName layerMapPath = do
  contents <- BS.readFile path
  lm       <- readLayerMap layerMapPath
  let cells = parseCells (buildForest (parseAllRecords contents))
  case find (\(Cell nm _ _ _ _) -> nm == cellName) cells of
    Just root -> putStr (renderConnectivitySummary (connectivity lm cells root))
    Nothing   -> error ("connectivity: no such cell " ++ show cellName)

-- | The unordered pair of named layers each connected Polygon pair spans,
-- e.g. ("li1", "poly") for a Polygon on "poly" connected to one on "li1" -
-- sorted so the same pair of layers always groups together regardless of
-- which side of the connection either Polygon happened to be on.
canonicalLayerPair :: String -> String -> (String, String)
canonicalLayerPair a b
  | a <= b    = (a, b)
  | otherwise = (b, a)

-- | Every distinct physical connection found between two named layers
-- (Relationship.connectivity's adjacency list is symmetric - each
-- connection appears once from either endpoint's perspective - so only
-- the a < b half of each pair is kept, to count every connection exactly
-- once), grouped into "intra" (a Polygon connected to another on the very
-- same named layer, i.e. a direct connection) and "inter" (two different
-- named layers, bridged by a cross connection's via) counts, plus the
-- grand total connection count.
summarizeConnectivity :: Map.Map LayerPolygon [LayerPolygon] -> ([(String, Int)], [((String, String), Int)], Int)
summarizeConnectivity conn = (intra, inter, length edges)
  where
    edges =
      [ (a, b)
      | (a, neighbors) <- Map.toList conn
      , b               <- neighbors
      , a < b
      ]
    counts = Map.fromListWith (+)
      [ (canonicalLayerPair lyrA lyrB, 1 :: Int)
      | (LayerPolygon lyrA _, LayerPolygon lyrB _) <- edges
      ]
    intra = [ (lyrA, n) | ((lyrA, lyrB), n) <- Map.toAscList counts, lyrA == lyrB ]
    inter = [ (pair, n) | (pair, n) <- Map.toAscList counts, fst pair /= snd pair ]

-- | Renders rows of cells as a simple space-aligned table with a header
-- row, each column padded to the width of its widest cell (header
-- included).
renderTable :: [String] -> [[String]] -> [String]
renderTable headerRow rows = map renderRow (headerRow : rows)
  where
    widths = map (maximum . map length) (transpose (headerRow : rows))
    renderRow cells = intercalate "  " (zipWith pad cells widths)
    pad cell w = cell ++ replicate (w - length cell) ' '

-- | Renders the intra-layer and inter-layer connectivity tables, plus the
-- total connection count, e.g.:
--
-- > Intra-layer connectivity:
-- >   Layer  Connections
-- >   li1    2
-- >   poly   3
-- >
-- > Inter-layer connectivity:
-- >   Layer A  Layer B  Connections
-- >   li1      met1     4
-- >   li1      poly     5
-- >
-- > Total connections: 14
renderConnectivitySummary :: Map.Map LayerPolygon [LayerPolygon] -> String
renderConnectivitySummary conn = unlines $
  [ "Intra-layer connectivity:" ]
  ++ map ("  " ++) (renderTable ["Layer", "Connections"] intraRows)
  ++ [ "", "Inter-layer connectivity:" ]
  ++ map ("  " ++) (renderTable ["Layer A", "Layer B", "Connections"] interRows)
  ++ [ "", "Total connections: " ++ show total ]
  where
    (intra, inter, total) = summarizeConnectivity conn
    intraRows = [ [lyr, show n] | (lyr, n) <- intra ]
    interRows = [ [lyrA, lyrB, show n] | ((lyrA, lyrB), n) <- inter ]

runConnectedComponents :: FilePath -> String -> FilePath -> IO ()
runConnectedComponents path cellName layerMapPath = do
  contents <- BS.readFile path
  lm       <- readLayerMap layerMapPath
  let cells = parseCells (buildForest (parseAllRecords contents))
  case find (\(Cell nm _ _ _ _) -> nm == cellName) cells of
    Just root -> putStr (renderComponentsSummary (connectivity lm cells root))
    Nothing   -> error ("components: no such cell " ++ show cellName)

-- | The number of distinct connected components (nets) - the number of
-- distinct component ids 'Relationship.connectedComponents' assigns,
-- found by deduplicating them through a Set rather than relying on them
-- being contiguous.
numComponents :: Map.Map LayerPolygon Int -> Int
numComponents = Set.size . Set.fromList . Map.elems

-- | Renders the node count (every Polygon with at least one connection -
-- 'Relationship.connectivity' never keys an isolated Polygon), the edge
-- count (reusing 'summarizeConnectivity''s already-deduplicated total,
-- since its adjacency list is symmetric), and the connected-component
-- count, e.g.:
--
-- > Nodes                : 42
-- > Edges                : 37
-- > Connected components : 5
renderComponentsSummary :: Map.Map LayerPolygon [LayerPolygon] -> String
renderComponentsSummary conn = renderIntersectionSummary
  [ ("Nodes", Map.size conn)
  , ("Edges", edgeCount)
  , ("Connected components", numComponents (connectedComponents conn))
  ]
  where
    (_, _, edgeCount) = summarizeConnectivity conn

runPinDump :: FilePath -> String -> FilePath -> Bool -> IO ()
runPinDump path cellName layerMapPath mergeInstances = do
  contents <- BS.readFile path
  lm       <- readLayerMap layerMapPath
  let cells = parseCells (buildForest (parseAllRecords contents))
  case find (\(Cell nm _ _ _ _) -> nm == cellName) cells of
    Just root
      | mergeInstances -> mapM_ putStrLn (renderPinsByCell lm (pins lm cells root))
      | otherwise       -> mapM_ putStrLn (renderPinsByInstance lm (pinsByInstance lm cells root))
    Nothing -> error ("pins: no such cell " ++ show cellName)

-- | A Layer's display label: the name of the LayerMap entry with exactly
-- this (layer, datatype) pair, plus the raw (index, kind) numbers in
-- brackets - e.g. "li1.pin (67, 16)" - or, if no such entry exists, just
-- the bracketed numbers.
layerLabel :: LayerMap -> Layer -> String
layerLabel (LayerMap entries _ _) (Layer idx knd) =
  maybe "" (++ " ") nameM ++ "(" ++ show idx ++ ", " ++ show knd ++ ")"
  where
    nameM = listToMaybe
      [ nm | LayerEntry nm lyrNum dt <- entries, lyrNum == idx, dt == knd ]

-- | Renders every instance's Pins (Relationship.pinsByInstance's
-- grouping - the root Cell's own name, or an SREF-placement-qualified
-- label for each transitively-reached instance, so two placements of the
-- very same Cell are dumped as separate groups) as a header line per
-- instance followed by one indented line per Pin naming its net, Layer
-- (see 'layerLabel'), and tracing its Polygon's vertex ring, instances in
-- ascending order of their label, with a blank line between instances.
renderPinsByInstance :: LayerMap -> Map.Map String [Pin] -> [String]
renderPinsByInstance lm grouped = concatMap renderInstance (Map.toAscList grouped)
  where
    renderInstance (instLabel, ps) =
      (instLabel ++ ":") : map renderPin ps ++ [""]
    renderPin (Pin lbl (LabeledPolygon poly _) lyr) =
      "  " ++ lbl ++ "  layer=" ++ layerLabel lm lyr ++ "  " ++ showCoordinates (polygonVertices poly)

-- | Renders every Cell's Pins once, treating every instantiation (direct
-- or via SREF) of the same Cell as the same: Pins are grouped by their
-- owning Cell's bare name (LabeledPolygon.parent) rather than by instance,
-- and deduplicated by (net name, Layer) - since a Cell's set of pins is a
-- property of its own definition, identical across every instantiation -
-- without a Polygon boundary, which would differ, meaninglessly, per
-- instance.
renderPinsByCell :: LayerMap -> [Pin] -> [String]
renderPinsByCell lm allPins = concatMap renderCell (Map.toAscList grouped)
  where
    grouped :: Map.Map String (Set.Set (String, Layer))
    grouped = Map.fromListWith Set.union
      [ (prnt, Set.singleton (lbl, lyr)) | Pin lbl (LabeledPolygon _ prnt) lyr <- allPins ]
    renderCell (cellName, pinSet) =
      (cellName ++ ":") : map renderPin (Set.toAscList pinSet) ++ [""]
    renderPin (lbl, lyr) = "  " ++ lbl ++ "  layer=" ++ layerLabel lm lyr

runNetlist :: FilePath -> String -> FilePath -> FilePath -> String -> IO ()
runNetlist path cellName layerMapPath componentsPath outputPin = do
  contents <- BS.readFile path
  lm       <- readLayerMap layerMapPath
  compList <- readComponentList componentsPath
  let cells = parseCells (buildForest (parseAllRecords contents))
  case find (\(Cell nm _ _ _ _) -> nm == cellName) cells of
    Just root -> mapM_ putStrLn (renderNetlist (netlist lm compList cells root outputPin))
    Nothing   -> error ("netlist: no such cell " ++ show cellName)

-- | Renders every <component.pin> -- <component.pin> connection, one per
-- line, in the Set's own ascending order (each pair already canonicalized
-- by 'Relationship.netlist', so this is a stable, duplicate-free order).
renderNetlist :: Set.Set ((String, String), (String, String)) -> [String]
renderNetlist = map renderEdge . Set.toList
  where
    renderEdge ((c1, p1), (c2, p2)) = c1 ++ "." ++ p1 ++ " -- " ++ c2 ++ "." ++ p2

runVerilog :: FilePath -> FilePath -> FilePath -> IO ()
runVerilog componentsPath netlistPath outputPath = do
  compList <- readComponentList componentsPath
  nl       <- readNetlist netlistPath
  writeFile outputPath (netlistToVerilog compList nl)

runLabels :: FilePath -> IO ()
runLabels path = do
  contents <- BS.readFile path
  let cells = parseCells (buildForest (parseAllRecords contents))
  mapM_ putStrLn (renderLabels cells)

-- | Every string block's label (the STRING record's payload), one section
-- per Cell in the file, listing only the Text elements the Cell directly
-- contains - elements pulled in transitively through an SREF are attributed
-- to the Cell that actually defines them, not expanded into every Cell that
-- references them.
renderLabels :: [Cell] -> [String]
renderLabels = concatMap renderCellLabels

renderCellLabels :: Cell -> [String]
renderCellLabels (Cell nm _ _ txts _) =
  (nm ++ ":") : map renderLabel txts ++ [""]
  where
    renderLabel (TextDescription _ _ val _ _ _ _) = "  " ++ val

-- | Maps GDS database units onto SVG pixels: scaled to fit the larger of
-- the drawing's width/height into 'targetSize' pixels, with 'svgPadding'
-- pixels of margin, and the y axis flipped - GDS y grows upward (see
-- 'Geom.boundingRect''s convention, where "top" is the *maximum* y), SVG y
-- grows downward.
data SvgTransform = SvgTransform
  { svgScale   :: Double
  , svgMinX    :: Int
  , svgMaxY    :: Int
  , svgPadding :: Double
  }

targetSize :: Double
targetSize = 900

mkTransform :: (Int, Int, Int, Int) -> SvgTransform
mkTransform (minX, _, maxX, maxY) = SvgTransform
  { svgScale   = targetSize / fromIntegral (max 1 (maxX - minX))
  , svgMinX    = minX
  , svgMaxY    = maxY
  , svgPadding = 30
  }

-- | The (minX, minY, maxX, maxY) bounds of every vertex of every given
-- polygon, defaulting to a fixed placeholder box when there are none (e.g.
-- an empty cell), so the SVG is still well-formed rather than crashing.
boundingBoxOf :: [Polygon] -> (Int, Int, Int, Int)
boundingBoxOf polys = case concatMap polygonVertices polys of
  []                     -> (0, 0, 100, 100)
  (Coordinate x0 y0 : cs) -> foldl' step (x0, y0, x0, y0) cs
  where
    step (minX, minY, maxX, maxY) (Coordinate x y) =
      (min minX x, min minY y, max maxX x, max maxY y)

transformPoint :: SvgTransform -> Coordinate -> (Double, Double)
transformPoint t (Coordinate x y) =
  ( fromIntegral (x - svgMinX t) * svgScale t + svgPadding t
  , fromIntegral (svgMaxY t - y) * svgScale t + svgPadding t
  )

canvasSize :: SvgTransform -> (Int, Int, Int, Int) -> (Double, Double)
canvasSize t (minX, minY, maxX, maxY) =
  ( fromIntegral (maxX - minX) * svgScale t + 2 * svgPadding t
  , fromIntegral (maxY - minY) * svgScale t + 2 * svgPadding t
  )

-- | A single filled/stroked <polygon> tracing a Polygon's ring.
svgPolygon :: SvgTransform -> String -> String -> Polygon -> String
svgPolygon t fill stroke p =
  "  <polygon points=\"" ++ points ++ "\" fill=\"" ++ fill
    ++ "\" stroke=\"" ++ stroke ++ "\" stroke-width=\"1.5\" />"
  where
    points = unwords [ printf "%.2f,%.2f" px py | c <- polygonVertices p, let (px, py) = transformPoint t c ]

-- | Renders the full SVG diagram: layer 1 shapes, then layer 2 shapes, then
-- the true intersection regions on top (so overlaps are visibly
-- highlighted), plus a small legend with per-layer shape counts.
renderSvg :: String -> [Polygon] -> [Polygon] -> [Polygon] -> String
renderSvg cellName layer1 layer2 intersections = unlines $
  [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
  , printf "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"%.2f\" height=\"%.2f\" viewBox=\"0 0 %.2f %.2f\">" cw ch cw ch
  , "  <title>" ++ escapeXml cellName ++ " intersections</title>"
  , printf "  <rect x=\"0\" y=\"0\" width=\"%.2f\" height=\"%.2f\" fill=\"white\" />" cw ch
  ]
  ++ map (svgPolygon t layer1Fill layer1Stroke) layer1
  ++ map (svgPolygon t layer2Fill layer2Stroke) layer2
  ++ map (svgPolygon t intersectFill intersectStroke) intersections
  ++ renderLegend cellName (length layer1) (length layer2) (length intersections)
  ++ [ "</svg>" ]
  where
    bbox     = boundingBoxOf (layer1 ++ layer2)
    t        = mkTransform bbox
    (cw, ch) = canvasSize t bbox
    layer1Fill     = "rgba(70,130,180,0.35)"; layer1Stroke     = "steelblue"
    layer2Fill     = "rgba(220,20,60,0.35)";  layer2Stroke     = "crimson"
    intersectFill  = "rgba(255,215,0,0.85)";  intersectStroke  = "darkorange"

renderLegend :: String -> Int -> Int -> Int -> [String]
renderLegend cellName n1 n2 n3 =
  [ "  <g font-family=\"sans-serif\" font-size=\"14\">"
  , "    <text x=\"12\" y=\"20\" font-weight=\"bold\">" ++ escapeXml cellName ++ "</text>"
  ]
  ++ concatMap legendRow (zip [0 ..] rows)
  ++ [ "  </g>" ]
  where
    rows =
      [ ("steelblue",  "Layer 1 shapes (" ++ show n1 ++ ")")
      , ("crimson",    "Layer 2 shapes (" ++ show n2 ++ ")")
      , ("darkorange", "True intersections (" ++ show n3 ++ ")")
      ]
    legendRow (i, (color, label)) =
      [ "    <rect x=\"12\" y=\"" ++ show (y - 12) ++ "\" width=\"12\" height=\"12\" fill=\"" ++ color ++ "\" />"
      , "    <text x=\"30\" y=\"" ++ show y ++ "\">" ++ escapeXml label ++ "</text>"
      ]
      where y = 40 + i * 20 :: Int

escapeXml :: String -> String
escapeXml = concatMap esc
  where
    esc '&' = "&amp;"
    esc '<' = "&lt;"
    esc '>' = "&gt;"
    esc '"' = "&quot;"
    esc c   = [c]

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
