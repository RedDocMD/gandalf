module Main where

import           AST                        (AST (..), astKind, buildForest,
                                              isCloser, isOpener)
import qualified Data.Attoparsec.ByteString as DAP
import qualified Data.ByteString            as BS
import           Data.List                  (intercalate)
import qualified Data.Map.Strict            as Map
import qualified Data.Set                   as Set
import           Options.Applicative
import           Parse                      (GdsRecord, GdsRecordT,
                                              parseGdsRecord)

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
  mapM_ putStrLn (renderGdsForest (buildForest (parseAllRecords contents)))

runElstr :: FilePath -> IO ()
runElstr path = do
  contents <- BS.readFile path
  let forest = buildForest (parseAllRecords contents)
  mapM_ putStrLn (renderElstr (collectUnitShapes forest))

-- | Renders a forest using the same box-drawing style as the Unix `tree`
-- command: "├── " / "└── " connectors and "│   " / "    " continuations.
renderGdsForest :: [AST] -> [String]
renderGdsForest ts = concatMap (\(t, isLast) -> renderGdsTree "" isLast t) (markLast ts)

renderGdsTree :: String -> Bool -> AST -> [String]
renderGdsTree prefix isLast (AST r children) =
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
collectUnitShapes :: [AST] -> Map.Map GdsRecord (Set.Set (Set.Set GdsRecord))
collectUnitShapes forest = Map.fromListWith Set.union
  [ (kind, Set.singleton shape) | (kind, shape) <- unitShapes forest ]

unitShapes :: [AST] -> [(GdsRecord, Set.Set GdsRecord)]
unitShapes = concatMap go
  where
    go node@(AST r children) =
      [ (astKind node, shape) | isOpener r ] ++ unitShapes children
      where
        shape = Set.fromList
          [ astKind c | c@(AST cr _) <- children, not (isCloser cr) ]

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
