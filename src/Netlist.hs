{-# LANGUAGE OverloadedStrings #-}

-- | Parses an extracted-netlist text file: one "A -- B" connection per
-- line, where each endpoint is either a bare "<module>.<pin>" top-level
-- port or a placed standard-cell instance's pin, e.g.
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

-- | A standard-cell instance as placed on the layout: its cell type, its
-- (x, y) placement, and the optional "rot=" / "mirrored" transform
-- attributes attached after the coordinate, e.g.
-- "sky130_fd_sc_hd__mux2_1@(29900,70720),rot=180.0,mirrored". When present,
-- "rot=" always precedes "mirrored".
data Instance = Instance
  { cellType :: String
  , position :: Coordinate
  , rotation :: Maybe Double
  , mirrored :: Bool
  } deriving (Show, Eq, Ord)

-- | One side of a "--" connection: either a pin directly on the top-level
-- module (module name, pin name) or a pin on a placed 'Instance'.
data Endpoint
  = ModulePin String String
  | InstancePin Instance String
  deriving (Show, Eq, Ord)

-- | A single parsed connection line: an unordered pair of Endpoints
-- sharing a net.
data NetEdge = NetEdge
  { from :: Endpoint
  , to   :: Endpoint
  } deriving (Show, Eq)

type Netlist = [NetEdge]

-- | A module name, cell type, or plain pin name: letters, digits, and
-- underscores (sky130 cell types use runs of consecutive underscores,
-- e.g. "sky130_fd_sc_hd__mux2_1", so this happily accepts those too).
identifier :: Parser String
identifier = decodeUtf8 <$> takeWhile1 (\c -> isAlphaNum c || c == '_')

-- | A pin name, with an optional trailing bus-bit index, e.g. "O[3]" for
-- a top-level module port bit.
pinName :: Parser String
pinName = do
  base <- identifier
  ix   <- optional (char '[' *> decimal <* char ']')
  pure (maybe base (\n -> base ++ "[" ++ show (n :: Int) ++ "]") ix)

-- | An instance's "(x,y)" placement coordinate.
coordinate :: Parser Coordinate
coordinate = do
  _  <- char '('
  px <- signed decimal
  _  <- char ','
  py <- signed decimal
  _  <- char ')'
  pure (Coordinate px py)

-- | The optional ",rot=<degrees>" and ",mirrored" transform attributes
-- following an instance's coordinate.
attributes :: Parser (Maybe Double, Bool)
attributes = do
  rot <- optional (string ",rot=" *> double)
  mir <- (True <$ string ",mirrored") <|> pure False
  pure (rot, mir)

-- | One "<module>.<pin>" or "<celltype>@(x,y)[,rot=..][,mirrored].<pin>"
-- endpoint.
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

-- | Parses a full netlist file's contents: one 'NetEdge' per line, with or
-- without a trailing newline at end of file.
parseNetlist :: Parser Netlist
parseNetlist = many1' (netEdge <* (endOfLine <|> endOfInput))

-- | Reads and parses a netlist text file, erroring out with attoparsec's
-- parse failure message if it's malformed.
readNetlist :: FilePath -> IO Netlist
readNetlist path = do
  contents <- readFileBS path
  case parseOnly (parseNetlist <* endOfInput) contents of
    Right nl  -> pure nl
    Left  err -> error (toText ("readNetlist: failed to parse " ++ path ++ ": " ++ err))

-- Rendering: Netlist -> Verilog

-- | Data.List's 'unlines', not relude's - relude's own is 'Text'-specific,
-- while this module's output is a plain 'String'.
unlines' :: [String] -> String
unlines' = concatMap (++ "\n")

isModulePin :: Endpoint -> Bool
isModulePin (ModulePin _ _) = True
isModulePin (InstancePin _ _) = False

-- | Every electrically distinct net among a Netlist's Endpoints: the
-- undirected graph of NetEdges, split into its connected components (so a
-- net spanning more than one NetEdge line - e.g. "A -- B" and "B -- C" -
-- is one net, not two), each returned as its own group of Endpoints. Built
-- by the same adjacency-list-plus-DFS technique as
-- 'Relationship.connectedComponents'.
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

-- | The Verilog identifier a net (a connected group of Endpoints, per
-- 'nets') is referred to by: the shared pin name, for a net that includes
-- one or more top-level ModulePin Endpoints (assumed, since they landed on
-- the same net, to already share one pin name - the point of them being on
-- the same net at all), or else a fresh "netN" wire name for a net that
-- only connects Instance pins to each other.
netSignal :: Int -> [Endpoint] -> String
netSignal idx eps = case [ pin | ModulePin _ pin <- eps ] of
  (pin : _) -> pin
  []        -> "net" ++ show idx

-- | Every Endpoint's Verilog signal name, per 'netSignal', indexed by the
-- Endpoint itself for lookup while rendering instances.
endpointSignals :: [[Endpoint]] -> Map.Map Endpoint String
endpointSignals groups = Map.fromList
  [ (ep, netSignal idx eps) | (idx, eps) <- zip [0 :: Int ..] groups, ep <- eps ]

-- | Every distinct 'Instance' placed within a Netlist, deduplicated (an
-- Instance normally appears on more than one NetEdge - once per pin it has
-- a connection for) and sorted into a stable order so the instance names
-- 'instanceNames' assigns stay deterministic across runs.
netlistInstances :: Netlist -> [Instance]
netlistInstances edges = sortOn (\i -> (cellType i, position i, rotation i, mirrored i))
  (ordNub [ inst | NetEdge a b <- edges, InstancePin inst _ <- [a, b] ])

-- | A stable, unique Verilog instance name for every given Instance -
-- "<cellType>_<n>", numbered in list order - since an Instance's own
-- coordinate/rotation/mirroring attributes aren't themselves valid (or,
-- for coordinates alone, guaranteed unique) Verilog identifier text.
instanceNames :: [Instance] -> Map.Map Instance String
instanceNames insts = Map.fromList
  [ (inst, cellType inst ++ "_" ++ show n) | (inst, n) <- zip insts [0 :: Int ..] ]

-- | Every cell type named in a ComponentList's entries - matched by each
-- entry's own 'Component.componentType', not the ComponentList's own key,
-- since an Instance is identified by its cell type (see 'Instance'), not
-- by any instance name of its own - mapped to its full formal pin name
-- list, in the order given in the YAML file.
cellTypePins :: ComponentList -> Map.Map String [String]
cellTypePins compList = Map.fromList
  [ (ct, map declaredPinName ps)
  | Component { componentType = ct, pins = ps } <- Map.elems compList
  ]
  where declaredPinName Pin { name = n } = n

-- | Every Instance's own pin -> net-signal-name connections, gathered in a
-- single pass over the Netlist's Endpoints.
instancePinSignals :: Netlist -> Map.Map Endpoint String -> Map.Map Instance (Map.Map String String)
instancePinSignals edges sigs = Map.fromListWith Map.union
  [ (inst, Map.singleton pin (sigs Map.! ep))
  | NetEdge a b <- edges
  , ep@(InstancePin inst pin) <- [a, b]
  ]

-- | Renders one Instance's Verilog instantiation: its cell type as the
-- module name (per 'netlistToVerilog''s assumption that it's externally
-- defined), a name from 'instanceNames', and one ".pin(net)" per formal
-- pin - from 'compList' where the Instance's cell type is found there, or
-- else just whichever pins the Netlist itself connects, in ascending
-- order. A formal pin the Netlist never connects is instantiated as
-- explicitly floating (".pin()").
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

-- | Renders a Netlist as a Verilog module: one port per top-level pin
-- (declared "inout", since a plain "A -- B" connection carries no
-- direction information to distinguish input from output), one wire per
-- otherwise-unnamed net, and one instantiation per placed 'Instance' -
-- naming each instantiated Verilog module after its 'cellType' string, per
-- this function's assumption that every distinct cell type already exists
-- as an externally-defined Verilog module with matching port names. A
-- bus-bit pin like "O[3]" (see 'pinName') is treated as its own
-- independent single-bit port/wire, not as one bit of a wider "O" vector -
-- the parsed netlist carries no bus-width information to reconstruct that
-- with.
--
-- 'compList' supplies each cell type's full formal pin list (see
-- 'cellTypePins'): every one of those pins is instantiated for every
-- Instance of that cell type, connected to its net where the Netlist
-- connects it, and left explicitly floating otherwise - see
-- 'renderInstance'.
--
-- Errors if the Netlist has no top-level ModulePin at all, or if its
-- ModulePins name more than one distinct top-level module - a single
-- extracted-netlist file (see this module's own top-level documentation)
-- is only ever traced from one such module.
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
