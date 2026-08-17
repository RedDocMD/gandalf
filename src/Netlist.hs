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
  ) where

import           Data.Attoparsec.ByteString.Char8 (Parser, char, decimal,
                                                     double, endOfInput,
                                                     endOfLine, many1',
                                                     parseOnly, signed, string,
                                                     takeWhile1)
import           Data.Char                         (isAlphaNum)
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
  } deriving (Show, Eq)

-- | One side of a "--" connection: either a pin directly on the top-level
-- module (module name, pin name) or a pin on a placed 'Instance'.
data Endpoint
  = ModulePin String String
  | InstancePin Instance String
  deriving (Show, Eq)

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
