module Main where

import qualified Data.Attoparsec.ByteString as DAP
import qualified Data.ByteString            as BS
import           Data.List                  (foldl', intercalate)
import qualified Data.Map.Strict            as Map
import qualified Data.Set                   as Set
import           Options.Applicative
import           Parse                      (GdsRecord, GdsRecordT (..),
                                              gdsRecordKind, parseGdsRecord)

data Command = Dump FilePath | Elstr FilePath

commandParser :: Parser Command
commandParser = subparser
  ( command "dump" (info (dumpParser <**> helper) (progDesc "Parse a GDS file and print its records"))
 <> command "elstr" (info (elstrParser <**> helper) (progDesc "Summarize the unique sets of sub-units found within each hierarchical GDS unit")) )

dumpParser :: Parser Command
dumpParser = Dump <$> fileArgument

elstrParser :: Parser Command
elstrParser = Elstr <$> fileArgument

fileArgument :: Parser FilePath
fileArgument = argument str (metavar "FILE" <> help "Path to a GDS file")

main :: IO ()
main = do
  cmd <- execParser opts
  case cmd of
    Dump path  -> runDump path
    Elstr path -> runElstr path
  where
    opts = info (commandParser <**> helper)
      ( fullDesc <> progDesc "Gandalf: parse and inspect GDSII files" )

runDump :: FilePath -> IO ()
runDump path = do
  contents <- BS.readFile path
  mapM_ putStrLn (renderGdsForest (buildGdsForest (parseAllRecords contents)))

runElstr :: FilePath -> IO ()
runElstr path = do
  contents <- BS.readFile path
  let forest = buildGdsForest (parseAllRecords contents)
  mapM_ putStrLn (renderElstr (collectUnitShapes forest))

-- | A GDS record together with the records nested beneath it, e.g. the
-- elements between a BOUNDARY and its ENDEL, or the structures between a
-- BGNLIB and its ENDLIB.
data GdsTree = GdsTree GdsRecordT [GdsTree]

-- | Groups a flat stream of records into a forest by matching each
-- "begin" record (BGNLIB, BGNSTR, or an element record) with the "end"
-- record that closes it (ENDLIB, ENDSTR, ENDEL). A stray end record with
-- no matching begin, or a begin record left open at end of input, is
-- handled gracefully rather than treated as an error, since this is a
-- display concern rather than a validity check.
buildGdsForest :: [GdsRecordT] -> [GdsTree]
buildGdsForest = finalize . foldl' step ([], [])
  where
    step (forest, stack) r
      | isGdsOpener r = (forest, (r, []) : stack)
      | isGdsCloser r = case stack of
          (rec0, kids) : rest -> closeInto rest forest rec0 (GdsTree r [] : kids)
          []                  -> (GdsTree r [] : forest, [])
      | otherwise = case stack of
          (rec0, kids) : rest -> (forest, (rec0, GdsTree r [] : kids) : rest)
          []                  -> (GdsTree r [] : forest, [])

    closeInto rest forest rec0 kids = case rest of
      (rec1, kids1) : rest' -> (forest, (rec1, GdsTree rec0 (reverse kids) : kids1) : rest')
      []                    -> (GdsTree rec0 (reverse kids) : forest, [])

    finalize (forest, [])                = reverse forest
    finalize (forest, (rec0, kids) : rest) = finalize (closeInto rest forest rec0 kids)

isGdsOpener :: GdsRecordT -> Bool
isGdsOpener r = case r of
  GdsBgnLibT{}  -> True
  GdsBgnStrT{}  -> True
  GdsBoundaryT  -> True
  GdsPathT      -> True
  GdsSrefT      -> True
  GdsArefT      -> True
  GdsTextT      -> True
  _             -> False

isGdsCloser :: GdsRecordT -> Bool
isGdsCloser r = case r of
  GdsEndLibT -> True
  GdsEndStrT -> True
  GdsEndElT  -> True
  _          -> False

-- | Renders a forest using the same box-drawing style as the Unix `tree`
-- command: "├── " / "└── " connectors and "│   " / "    " continuations.
renderGdsForest :: [GdsTree] -> [String]
renderGdsForest ts = concatMap (\(t, isLast) -> renderGdsTree "" isLast t) (markLast ts)

renderGdsTree :: String -> Bool -> GdsTree -> [String]
renderGdsTree prefix isLast (GdsTree r children) =
  (prefix ++ connector ++ show r) :
  concatMap (\(c, isLastChild) -> renderGdsTree childPrefix isLastChild c) (markLast children)
  where
    connector   = if isLast then "└── " else "├── "
    childPrefix = prefix ++ if isLast then "    " else "│   "

markLast :: [a] -> [(a, Bool)]
markLast []       = []
markLast [x]      = [(x, True)]
markLast (x : xs) = (x, False) : markLast xs

-- | For each hierarchical unit kind (BGNSTR, BOUNDARY, ...), the distinct
-- sets of immediate sub-unit kinds seen across all its instances in the
-- file. The closing record (ENDEL/ENDSTR/ENDLIB) is always present and so
-- isn't interesting; it's excluded from the shape.
collectUnitShapes :: [GdsTree] -> Map.Map GdsRecord (Set.Set (Set.Set GdsRecord))
collectUnitShapes forest = Map.fromListWith Set.union
  [ (kind, Set.singleton shape) | (kind, shape) <- unitShapes forest ]

unitShapes :: [GdsTree] -> [(GdsRecord, Set.Set GdsRecord)]
unitShapes = concatMap go
  where
    go (GdsTree r children) =
      [ (gdsRecordKind r, shape) | isGdsOpener r ] ++ unitShapes children
      where
        shape = Set.fromList
          [ gdsRecordKind cr | GdsTree cr _ <- children, not (isGdsCloser cr) ]

renderElstr :: Map.Map GdsRecord (Set.Set (Set.Set GdsRecord)) -> [String]
renderElstr = concatMap renderUnit . Map.toAscList
  where
    renderUnit (kind, shapes) =
      show kind : map (("  " ++) . renderShape) (Set.toAscList shapes)
    renderShape shape = "{" ++ intercalate ", " (map show (Set.toAscList shape)) ++ "}"

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
