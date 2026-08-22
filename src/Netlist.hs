{-# LANGUAGE OverloadedStrings #-}

-- | Parses an extracted-netlist text file: one "A -- B" connection per
-- line, each endpoint a bare "<module>.<pin>" top-level port or a placed
-- standard-cell instance's pin, e.g.
--
-- > adder_demo.clk -- sky130_fd_sc_hd__clkbuf_16@(31740,48960).A
-- > sky130_fd_sc_hd__a31o_2@(62560,27200),mirrored.A1 -- sky130_fd_sc_hd__and2_2@(69920,16320).A
module Netlist
  ( Instance (..)
  , Endpoint (..)
  , NetEdge (..)
  , Netlist
  , parseNetlist
  , readNetlist
  , netlistToVerilog
  ) where

import           Component                         (Component (..),
                                                     ComponentList, Pin (..))
import           Data.Attoparsec.ByteString.Char8 (Parser, char, decimal,
                                                     double, endOfInput,
                                                     endOfLine, many1',
                                                     parseOnly, signed, string,
                                                     takeWhile1)
import           Data.Char                         (isAlphaNum)
import qualified Data.Map.Strict                    as Map
import qualified Data.Set                           as Set
import           Structure                          (Coordinate (..))

-- | A placed standard-cell instance: cell type, (x, y) placement, and the
-- optional "rot=" / "mirrored" attributes after the coordinate, e.g.
-- "sky130_fd_sc_hd__mux2_1@(29900,70720),rot=180.0,mirrored" ("rot="
-- always precedes "mirrored" when both are present).
data Instance = Instance
  { cellType :: String
  , position :: Coordinate
  , rotation :: Maybe Double
  , mirrored :: Bool
  } deriving (Show, Eq, Ord)

-- | One side of a "--" connection: a top-level module pin (module name,
-- pin name), or a pin on a placed 'Instance'.
data Endpoint
  = ModulePin String String
  | InstancePin Instance String
  deriving (Show, Eq, Ord)

data NetEdge = NetEdge
  { from :: Endpoint
  , to   :: Endpoint
  } deriving (Show, Eq)

type Netlist = [NetEdge]

-- | A module name, cell type, or plain pin name: letters, digits, and
-- underscores (accepts sky130's runs of consecutive underscores, e.g.
-- "sky130_fd_sc_hd__mux2_1").
identifier :: Parser String
identifier = decodeUtf8 <$> takeWhile1 (\c -> isAlphaNum c || c == '_')

-- | A pin name with an optional trailing bus-bit index, e.g. "O[3]".
pinName :: Parser String
pinName = do
  base <- identifier
  ix   <- optional (char '[' *> decimal <* char ']')
  pure (maybe base (\n -> base ++ "[" ++ show (n :: Int) ++ "]") ix)

coordinate :: Parser Coordinate
coordinate = do
  _  <- char '('
  px <- signed decimal
  _  <- char ','
  py <- signed decimal
  _  <- char ')'
  pure (Coordinate px py)

attributes :: Parser (Maybe Double, Bool)
attributes = do
  rot <- optional (string ",rot=" *> double)
  mir <- (True <$ string ",mirrored") <|> pure False
  pure (rot, mir)

endpoint :: Parser Endpoint
endpoint = do
  ident <- identifier
  instancePin ident <|> modulePin ident
  where
    instancePin ident = do
      _          <- char '@'
      pos        <- coordinate
      (rot, mir) <- attributes
      _          <- char '.'
      InstancePin (Instance ident pos rot mir) <$> pinName
    modulePin ident = do
      _   <- char '.'
      ModulePin ident <$> pinName

netEdge :: Parser NetEdge
netEdge = NetEdge <$> endpoint <*> (string " -- " *> endpoint)

-- | One 'NetEdge' per line, with or without a trailing newline at EOF.
parseNetlist :: Parser Netlist
parseNetlist = many1' (netEdge <* (endOfLine <|> endOfInput))

-- | Errors with attoparsec's parse failure message if malformed.
readNetlist :: FilePath -> IO Netlist
readNetlist path = do
  contents <- readFileBS path
  case parseOnly (parseNetlist <* endOfInput) contents of
    Right nl  -> pure nl
    Left  err -> error (toText ("readNetlist: failed to parse " ++ path ++ ": " ++ err))

-- Rendering: Netlist -> Verilog

-- | Data.List's 'unlines', not relude's ('Text'-specific; output here is 'String').
unlines' :: [String] -> String
unlines' = concatMap (++ "\n")

isModulePin :: Endpoint -> Bool
isModulePin (ModulePin _ _) = True
isModulePin (InstancePin _ _) = False

-- | Every electrically distinct net: the undirected graph of NetEdges
-- split into connected components (so "A -- B" and "B -- C" form one net,
-- not two).
nets :: Netlist -> [[Endpoint]]
nets edges = Map.elems (Map.fromListWith (++) [ (cid, [ep]) | (ep, cid) <- Map.toList assigned ])
  where
    adj = Map.fromListWith (++) (concatMap symmetric edges)
    symmetric (NetEdge a b) = [(a, [b]), (b, [a])]

    assigned :: Map.Map Endpoint Int
    assigned = go (concatMap (\(NetEdge a b) -> [a, b]) edges) 0 Map.empty

    go :: [Endpoint] -> Int -> Map.Map Endpoint Int -> Map.Map Endpoint Int
    go [] _ acc = acc
    go (n : ns) cid acc
      | Map.member n acc = go ns cid acc
      | otherwise         = go ns (cid + 1) (dfs [n] Set.empty acc)
      where
        dfs :: [Endpoint] -> Set.Set Endpoint -> Map.Map Endpoint Int -> Map.Map Endpoint Int
        dfs [] _ a = a
        dfs (x : xs) seen a
          | x `Set.member` seen = dfs xs seen a
          | otherwise = dfs (Map.findWithDefault [] x adj ++ xs) (Set.insert x seen) (Map.insert x cid a)

-- | The Verilog identifier a net (per 'nets') is referred to by: the
-- shared pin name if it includes a ModulePin, else a fresh "netN" wire
-- name.
netSignal :: Int -> [Endpoint] -> String
netSignal idx eps = case [ pin | ModulePin _ pin <- eps ] of
  (pin : _) -> pin
  []        -> "net" ++ show idx

endpointSignals :: [[Endpoint]] -> Map.Map Endpoint String
endpointSignals groups = Map.fromList
  [ (ep, netSignal idx eps) | (idx, eps) <- zip [0 :: Int ..] groups, ep <- eps ]

-- | Every distinct 'Instance' placed within a Netlist, deduplicated and
-- sorted into a stable order so 'instanceNames' stays deterministic.
netlistInstances :: Netlist -> [Instance]
netlistInstances edges = sortOn (\i -> (cellType i, position i, rotation i, mirrored i))
  (ordNub [ inst | NetEdge a b <- edges, InstancePin inst _ <- [a, b] ])

-- | A stable, unique Verilog instance name per Instance: "<cellType>_<n>",
-- numbered in list order.
instanceNames :: [Instance] -> Map.Map Instance String
instanceNames insts = Map.fromList
  [ (inst, cellType inst ++ "_" ++ show n) | (inst, n) <- zip insts [0 :: Int ..] ]

-- | Each cell type - identified by its YAML entry key (e.g.
-- "sky130_fd_sc_hd__clkbuf_4"), matching 'Instance''s cellType, not the
-- entry's non-unique "type" description - mapped to its formal pin names
-- in YAML order.
cellTypePins :: ComponentList -> Map.Map String [String]
cellTypePins compList = Map.map (\Component { pins = ps } -> map declaredPinName ps) compList
  where declaredPinName Pin { name = n } = n

instancePinSignals :: Netlist -> Map.Map Endpoint String -> Map.Map Instance (Map.Map String String)
instancePinSignals edges sigs = Map.fromListWith Map.union
  [ (inst, Map.singleton pin (sigs Map.! ep))
  | NetEdge a b <- edges
  , ep@(InstancePin inst pin) <- [a, b]
  ]

-- | Renders one Instance's Verilog instantiation: cell type as module name,
-- a name from 'instanceNames', and one ".pin(net)" per formal pin (from
-- 'compList' if known, else whichever pins the Netlist connects). A formal
-- pin the Netlist never connects is left floating (".pin()").
renderInstance :: ComponentList -> Map.Map Instance (Map.Map String String) -> Map.Map Instance String
               -> Instance -> [String]
renderInstance compList pinSigs names inst =
  [ "  " ++ cellType inst ++ " " ++ instName ++ " ("
  , intercalate ",\n" (map renderPin formalPins)
  , "  );"
  , ""
  ]
  where
    instName   = names Map.! inst
    connected  = Map.findWithDefault Map.empty inst pinSigs
    formalPins = case Map.lookup (cellType inst) (cellTypePins compList) of
      Just ps -> ps
      Nothing -> Map.keys connected
    renderPin pin = "    ." ++ pin ++ "(" ++ Map.findWithDefault "" pin connected ++ ")"

-- | Renders a Netlist as a Verilog module: one "inout" port per top-level
-- pin (direction is unknown from a plain "A -- B" connection), one wire
-- per otherwise-unnamed net, and one instantiation per placed 'Instance',
-- assuming each 'cellType' already exists as an externally-defined
-- Verilog module with matching ports. A bus-bit pin like "O[3]" is treated
-- as its own independent single-bit port/wire, not part of a wider vector
-- - the netlist carries no bus-width information.
--
-- 'compList' supplies each cell type's formal pin list (see
-- 'cellTypePins'); see 'renderInstance' for how it's used.
--
-- Errors if the Netlist has no top-level ModulePin, or its ModulePins
-- name more than one top-level module.
netlistToVerilog :: ComponentList -> Netlist -> String
netlistToVerilog compList edges = unlines' $
  [ "module " ++ topModule ++ " ("
  , intercalate ",\n" (map ("    " ++) ports)
  , ");"
  ]
  ++ map (\p -> "  inout " ++ p ++ ";") ports
  ++ [ "" ]
  ++ map (\w -> "  wire " ++ w ++ ";") wires
  ++ [ "" ]
  ++ concatMap (renderInstance compList pinSigs names) insts
  ++ [ "endmodule" ]
  where
    topModule = case ordNub [ m | NetEdge a b <- edges, ModulePin m _ <- [a, b] ] of
      [m] -> m
      []  -> error (toText ("netlistToVerilog: no top-level module pins found in netlist" :: String))
      ms  -> error (toText ("netlistToVerilog: netlist spans more than one top-level module: " ++ show ms))

    groups  = nets edges
    sigs    = endpointSignals groups
    indexed = zip [0 :: Int ..] groups
    ports   = [ netSignal idx eps | (idx, eps) <- indexed, any isModulePin eps ]
    wires   = [ netSignal idx eps | (idx, eps) <- indexed, not (any isModulePin eps) ]
    insts   = netlistInstances edges
    names   = instanceNames insts
    pinSigs = instancePinSignals edges sigs
