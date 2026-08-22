{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}

-- | Relates a Cell's geometry to the Cells it comes from: for every layer,
-- the Polygons on it (including those pulled in through SREFs), each
-- labeled with the Cell it was directly defined in. Also relates named pin
-- layers (per a LayerMap) to the text-labeled Polygons that are their pins.
module Relationship
  ( LabeledPolygon (..)
  , layerPolygons
  , srefTransform
  , Pin (..)
  , pins
  , pinsByInstance
  , LayerPolygon (..)
  , connectivity
  , connectedComponents
  , netlist
  ) where

import qualified Component
import           Component       (ComponentList)
import           Data.List       (isSuffixOf)
import qualified Data.Map.Lazy   as MapLazy
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import           Geom            (Polygon, Rectangle (..),
                                  RectangleBounded (..), Transform (..),
                                  boundaryToPolygon, edgeTouches,
                                  pathToPolygon, polygonIntersection,
                                  transformCoordinate, transformPolygon)
import           LayerMap        (CrossConnection (..), DirectConnection (..),
                                  LayerEntry (..), LayerMap (..))
import           Parse           (GdsStransFlags (..))
import           Structure       (Boundary (..), Cell (..), CellRef (..),
                                  Coordinate (..), Layer (..), Path (..),
                                  TextDescription (..))

-- | A Polygon paired with the name of the Cell it was directly defined in
-- - the root Cell for its own Boundary/Path elements, or the Cell reached
-- transitively through SREF. Stable regardless of nesting depth: never
-- rewritten as it's carried upward through enclosing SREFs.
data LabeledPolygon = LabeledPolygon
  { polygon :: !Polygon
  , parent  :: !String
  } deriving (Show, Eq, Ord)

-- | Every Boundary/Path element reachable from a Cell (including through
-- SREFs), grouped by Layer, expressed in the Cell's own coordinate system
-- with each SREF's translation/rotation/mirroring applied, and labeled per
-- 'LabeledPolygon'.
--
-- Precondition: 'cells' contains every Cell transitively referenced from
-- 'root' - an SREF naming a missing Cell contributes no geometry.
--
-- Built by knot-tying: each Cell's polygons look up the memoized polygons
-- of the Cells it references from the very map being constructed, so
-- laziness resolves the recursion without a topological sort (would only
-- loop on a cyclic SREF chain, which GDS doesn't permit). Must use
-- Data.Map.Lazy: Strict's fromList forces each value into WHNF while
-- inserting, demanding the unfinished map's spine and throwing <<loop>>.
layerPolygons :: [Cell] -> Cell -> Map.Map Layer [LabeledPolygon]
layerPolygons cells root = perCell MapLazy.! root.name
  where
    perCell :: MapLazy.Map String (Map.Map Layer [LabeledPolygon])
    perCell = MapLazy.fromList [ (c.name, cellLayerPolygons c) | c <- cells ]

    -- LabeledPolygon's fields are strict, so Data.Map.Strict's fromListWith
    -- here never lets unevaluated polygons/labels build up behind a list.
    cellLayerPolygons :: Cell -> Map.Map Layer [LabeledPolygon]
    cellLayerPolygons c = Map.fromListWith (++) $
      [ (b.layer, [LabeledPolygon (boundaryToPolygon b) c.name]) | b <- c.boundary ]
      ++ [ (p.layer, [LabeledPolygon (pathToPolygon p) c.name]) | p <- c.path ]
      ++ [ (lyr, map (transformLabeled (srefTransform ref)) lps)
         | ref <- c.cellRef
         , (lyr, lps) <- Map.toList (MapLazy.findWithDefault Map.empty ref.name perCell)
         ]

-- | Uses the constructor rather than record update: Pin shares the
-- 'polygon' field name, which would make 'lp { polygon = ... }' ambiguous.
transformLabeled :: Transform -> LabeledPolygon -> LabeledPolygon
transformLabeled t (LabeledPolygon p prnt) = LabeledPolygon (transformPolygon t p) prnt

-- | An SREF's placement as a Geom.Transform, per GDS defaults: no STRANS
-- record means no reflection, no ANGLE record means no rotation.
srefTransform :: CellRef -> Transform
srefTransform ref = Transform
  { mirrorX  = maybe False stransMirrorX ref.translation
  , angleDeg = fromMaybe 0 ref.angle
  , offset   = ref.coord
  }
  where
    stransMirrorX (GdsStransFlags mx _ _) = mx

-- | A named pin: the Polygon a net-name text label sits inside, on some
-- named ".pin" layer. Keeps both the Polygon and its Layer on the record
-- so either is traceable without a separate lookup structure (Polygon has
-- no Ord instance to key a reverse map by).
data Pin = Pin
  { label   :: !String
  , polygon :: !LabeledPolygon
  , layer   :: !Layer
  } deriving (Show, Eq)

-- | Every pin found on a Cell: for every LayerMap entry whose name ends in
-- ".pin", every Polygon on that (index, datatype) Layer (via
-- 'layerPolygons') enclosing the anchor point of a text label sharing that
-- layer's GDS layer *number* - the sky130-style convention of matching a
-- pin's geometry to its net label purely by GDS layer number, ignoring
-- datatype/texttype on both sides. "Encloses" tests the Polygon's bounding
-- rectangle (see 'containsPoint'), since pin shapes are rectangles by PDK
-- convention.
--
-- An unlabeled pin Polygon, or a label enclosed by no pin Polygon,
-- contributes nothing.
pins :: LayerMap -> [Cell] -> Cell -> [Pin]
pins lm cells root =
  [ Pin { label = val, polygon = lp, layer = lyr }
  | lyr            <- pinLayers
  , lp             <- Map.findWithDefault [] lyr grouped
  , (coord, val)   <- Map.findWithDefault [] lyr.index textsByIndex
  , containsPoint (boundingRect lp.polygon) coord
  ]
  where
    grouped   = layerPolygons cells root
    pinLayers = [ Layer e.layer e.datatype | e <- lm.layers, ".pin" `isSuffixOf` e.name ]

    textsByIndex :: Map.Map Int [(Coordinate, String)]
    textsByIndex = Map.fromListWith (++)
      [ (lyr.index, [(coord, val)]) | (lyr, coord, val) <- cellTexts cells root ]

-- | Every TextDescription's Layer, anchor Coordinate and string value,
-- reachable from a Cell (including through SREFs), with each SREF's
-- placement applied to the anchor. Built by the same knot-tying technique
-- as 'layerPolygons' - see there for why Data.Map.Lazy is required.
cellTexts :: [Cell] -> Cell -> [(Layer, Coordinate, String)]
cellTexts cells root = perCell MapLazy.! root.name
  where
    perCell :: MapLazy.Map String [(Layer, Coordinate, String)]
    perCell = MapLazy.fromList [ (c.name, ownTexts c) | c <- cells ]

    ownTexts :: Cell -> [(Layer, Coordinate, String)]
    ownTexts c =
      [ (t.layer, t.coord, t.value) | t <- c.text ]
      ++ [ (lyr, transformCoordinate (srefTransform ref) coord, val)
         | ref <- c.cellRef
         , (lyr, coord, val) <- MapLazy.findWithDefault [] ref.name perCell
         ]

-- | Whether a Coordinate falls within (or on the border of) a Rectangle.
-- topLeft.y is the *maximum* y, per Geom.RectangleBounded's convention.
containsPoint :: Rectangle -> Coordinate -> Bool
containsPoint r c =
  c.x >= r.topLeft.x && c.x <= r.topLeft.x + r.width &&
  c.y <= r.topLeft.y && c.y >= r.topLeft.y - r.height

-- | A Polygon identified, for connectivity, by the named layer it was
-- gathered from - a LayerEntry name's dot-separated prefix (e.g. "poly"),
-- valid across every datatype under that name; see 'connectivity'.
data LayerPolygon = LayerPolygon
  { layerName :: !String
  , labeled   :: !LabeledPolygon
  } deriving (Show, Eq, Ord)

-- | Every Polygon on a Cell whose LayerEntry name has the given
-- dot-separated prefix, e.g. "poly" gathers "poly.drawing", "poly.gate",
-- "poly.res", ... regardless of datatype.
namedLayerPolygons :: Map.Map Layer [LabeledPolygon] -> LayerMap -> String -> [LayerPolygon]
namedLayerPolygons grouped lm nm =
  [ LayerPolygon nm lp
  | e  <- lm.layers
  , takeWhile (/= '.') e.name == nm
  , lp <- Map.findWithDefault [] (Layer e.layer e.datatype) grouped
  ]

-- | Whether two Polygons' bounding rectangles overlap (a touch counts) -
-- the precondition 'Geom.polygonIntersection' requires but doesn't check.
boundsOverlap :: Polygon -> Polygon -> Bool
boundsOverlap p q =
  r1.topLeft.x <= r2.topLeft.x + r2.width && r2.topLeft.x <= r1.topLeft.x + r1.width &&
  r1.topLeft.y >= r2.topLeft.y - r2.height && r2.topLeft.y >= r1.topLeft.y - r1.height
  where
    r1 = boundingRect p
    r2 = boundingRect q

-- | Whether two Polygons are electrically touching: a true (non-zero-area)
-- overlap, per 'Geom.polygonIntersection', or edge-to-edge abutment, per
-- 'Geom.edgeTouches'. Meeting only at a corner counts as neither.
overlaps :: Polygon -> Polygon -> Bool
overlaps p q = boundsOverlap p q && (isJust (polygonIntersection p q) || edgeTouches p q)

-- | Every pair of Polygons electrically connected within a Cell, per a
-- LayerMap's "direct_connections" (same-named-layer Polygons that touch or
-- overlap, per 'overlaps') and "cross_connections" (named layer A bridged
-- to named layer B via a shared touch/overlap with via layer C), returned
-- as an adjacency list.
--
-- A "layer name" here matches every LayerEntry whose name has it as a
-- dot-separated prefix - see 'namedLayerPolygons' (GDS shares one layer
-- number across every purpose of a named layer but uses a different
-- datatype per purpose).
--
-- For a cross connection A-B via C: records an A-B edge for every A/B pair
-- overlapping the same C Polygon - never A-C or B-C, since C only bridges
-- the two.
connectivity :: LayerMap -> [Cell] -> Cell -> Map.Map LayerPolygon [LayerPolygon]
connectivity lm cells root =
  Map.fromListWith (++) (concatMap symmetric (directEdges ++ crossEdges))
  where
    grouped = layerPolygons cells root
    onLayer = namedLayerPolygons grouped lm

    symmetric (a, b) = [(a, [b]), (b, [a])]

    directEdges :: [(LayerPolygon, LayerPolygon)]
    directEdges =
      [ (a, b)
      | dc          <- lm.directConnections
      , (a : rest)  <- tails (onLayer dc.layer)
      , b           <- rest
      , overlaps a.labeled.polygon b.labeled.polygon
      ]

    crossEdges :: [(LayerPolygon, LayerPolygon)]
    crossEdges = concatMap crossConnectionEdges lm.crossConnections

    crossConnectionEdges :: CrossConnection -> [(LayerPolygon, LayerPolygon)]
    crossConnectionEdges cc = case cc.layers of
      [nmA, nmB] ->
        [ (a, b)
        | a <- onLayer nmA
        , c <- onLayer cc.via
        , overlaps a.labeled.polygon c.labeled.polygon
        , b <- onLayer nmB
        , overlaps b.labeled.polygon c.labeled.polygon
        ]
      other -> error (toText
        ("connectivity: cross_connections entry must name exactly two layers, got " ++ show other))

-- | Every LayerPolygon in a 'connectivity' adjacency list, labeled with
-- its connected component id (arbitrary, distinguishing components only).
-- Explores each unvisited node by DFS over the adjacency list; a Polygon
-- appearing only in an edge list (never as a key) still gets a component
-- via 'Map.findWithDefault' defaulting to an empty edge list.
connectedComponents :: Map.Map LayerPolygon [LayerPolygon] -> Map.Map LayerPolygon Int
connectedComponents adj = go (Map.keys adj) 0 Map.empty
  where
    go :: [LayerPolygon] -> Int -> Map.Map LayerPolygon Int -> Map.Map LayerPolygon Int
    go [] _ assigned = assigned
    go (n : ns) cid assigned
      | Map.member n assigned = go ns cid assigned
      | otherwise             = go ns (cid + 1) (dfs [n] Set.empty assigned)
      where
        dfs :: [LayerPolygon] -> Set.Set LayerPolygon -> Map.Map LayerPolygon Int
            -> Map.Map LayerPolygon Int
        dfs [] _ acc = acc
        dfs (x : xs) seen acc
          | x `Set.member` seen = dfs xs seen acc
          | otherwise = dfs (Map.findWithDefault [] x adj ++ xs) (Set.insert x seen)
                            (Map.insert x cid acc)

-- | Every Pin on a Cell's own directly-defined geometry, not transitively
-- through SREFs - restricts 'pins' to just that Cell (an SREF to any
-- other Cell then contributes no geometry, per 'layerPolygons''s fallback
-- for a Cell missing from "cells").
localPins :: LayerMap -> Cell -> [Pin]
localPins lm c = pins lm [c] c

-- | A human-readable label for one SREF placement: referenced Cell name
-- plus placement coordinate and, if set, rotation/mirroring, e.g.
-- "LEAF@(100,200)" or "LEAF@(100,200),rot=90.0,mirrored". Two SREFs to the
-- same Cell get distinct labels as long as they're placed differently,
-- which 'pinsByInstance' relies on to keep instantiations separate.
instanceLabel :: CellRef -> String
instanceLabel ref =
  ref.name ++ "@(" ++ show ref.coord.x ++ "," ++ show ref.coord.y ++ ")" ++ rot ++ mirror
  where
    rot = case ref.angle of
      Just a | a /= 0 -> ",rot=" ++ show a
      _               -> ""
    mirror = if maybe False (\s -> s.gdsMirrorX) ref.translation then ",mirrored" else ""

-- | Same ambiguous-field-update reason as 'transformLabeled'.
transformPin :: Transform -> Pin -> Pin
transformPin t (Pin lbl (LabeledPolygon p prnt) lyr) =
  Pin lbl (LabeledPolygon (transformPolygon t p) prnt) lyr

-- | Every Pin reachable from a Cell (including through SREFs), grouped by
-- the specific instance it was directly defined on: the root Cell's own
-- name, or an SREF-placement-qualified label (see 'instanceLabel').
--
-- Unlike 'pins' (whose LabeledPolygon.parent is just the bare owning Cell
-- name), this keeps multiple instantiations of the same Cell as separate
-- groups, since each is a physically distinct placement.
pinsByInstance :: LayerMap -> [Cell] -> Cell -> Map.Map String [Pin]
pinsByInstance lm cells root =
  Map.fromListWith (++) [ (lbl, [p]) | (lbl, p) <- walk root.name root ]
  where
    cellByName :: Map.Map String Cell
    cellByName = Map.fromList [ (c.name, c) | c <- cells ]

    -- Every Pin reachable from Cell c, in c's local coordinate system,
    -- tagged with the instance label of whichever Cell (c, or an
    -- SREF-placed descendant) it was directly defined on. A descendant's
    -- Pins are transformed once per level bubbling up through each
    -- enclosing SREF, same as 'layerPolygons', but the origin label is
    -- carried up unchanged rather than collapsing to a bare Cell name.
    walk :: String -> Cell -> [(String, Pin)]
    walk selfLabel c =
      [ (selfLabel, p) | p <- localPins lm c ]
      ++ [ (lbl, transformPin (srefTransform ref) p)
         | ref <- c.cellRef
         , Just child <- [Map.lookup ref.name cellByName]
         , (lbl, p) <- walk (instanceLabel ref) child
         ]

-- | Every instance label reachable from a Cell (per 'pinsByInstance''s
-- grouping) paired with the name of the Cell it's an instance of, so a
-- 'netlist' lookup can tell what an instance label was placed *from*
-- without re-deriving it from the label string.
instanceTypes :: [Cell] -> Cell -> Map.Map String String
instanceTypes cells root = Map.fromList (walk root.name root.name root)
  where
    cellByName :: Map.Map String Cell
    cellByName = Map.fromList [ (c.name, c) | c <- cells ]

    walk :: String -> String -> Cell -> [(String, String)]
    walk selfLabel typeName c =
      (selfLabel, typeName) : concat
        [ walk (instanceLabel ref) ref.name child
        | ref <- c.cellRef
        , Just child <- [Map.lookup ref.name cellByName]
        ]

-- | The named-layer prefix (e.g. "li1") a Layer belongs to, per the
-- LayerMap entry with exactly this (layer, datatype) pair.
layerPrefixName :: LayerMap -> Layer -> Maybe String
layerPrefixName lm lyr = listToMaybe
  [ takeWhile (/= '.') e.name | e <- lm.layers, e.layer == lyr.index, e.datatype == lyr.kind ]

-- | The 'connectivity' graph node a Pin's Polygon corresponds to, if its
-- Layer belongs to some named layer.
pinLayerPolygon :: LayerMap -> Pin -> Maybe LayerPolygon
pinLayerPolygon lm p = (\nm -> LayerPolygon nm p.polygon) <$> layerPrefixName lm p.layer

-- | Every ((component, pin), (component, pin)) connection reachable from
-- a named pin on a Cell's own definition: from that pin's physically
-- connected net (a maximal set of touching Polygons, per 'connectivity'),
-- fan out to every other declared pin touching that net, then to each of
-- those pins' components' *other* declared pins - never tracing a
-- component's internal wiring - to explore whatever nets they touch, and
-- so on transitively. Every pair of declared pins sharing a net becomes
-- one connection; a net touching zero or one declared pins contributes
-- none.
--
-- "Declared" means the pin's owning instance's type has a ComponentList
-- entry naming that pin - see 'declaredPins'. A geometrically-found Pin
-- absent from its component's declared list (e.g. VPWR/VGND) is never a
-- netlist node, as endpoint or fan-out source. This is why a components
-- file is needed on top of the LayerMap: it says which SREF instances are
-- opaque "components" and which of their pins matter, versus plain
-- unrecognized geometry that only shapes nets.
--
-- Precondition: 'outputPinName' is declared among root's own pins (root
-- needs its own ComponentList entry, matching its Cell name).
netlist :: LayerMap -> ComponentList -> [Cell] -> Cell -> String -> Set.Set ((String, String), (String, String))
netlist lm compList cells root outputPinName = case startNode of
  Nothing   -> error (toText ("netlist: no such declared pin " ++ show outputPinName ++ " on cell " ++ show root.name))
  Just seed -> Set.fromList (go [seed] Set.empty [])
  where
    adj   = connectivity lm cells root
    types = instanceTypes cells root

    -- Every geometrically-found Pin (per 'pinsByInstance'), restricted to
    -- those named in their owning instance's components-file entry - an
    -- unrecognized SREF instance (no ComponentList entry) drops out
    -- entirely, since 'Map.findWithDefault' on a missing key yields none.
    declaredPins :: Map.Map String [Pin]
    declaredPins = Map.mapWithKey (\instLbl -> filter ((`Set.member` declaredNames instLbl) . (.label)))
                                   (pinsByInstance lm cells root)
      where
        declaredNames instLbl = case Map.lookup instLbl types >>= (`Map.lookup` compList) of
          Nothing   -> Set.empty
          Just comp -> Set.fromList [ p.name | p <- comp.pins ]

    startNode :: Maybe LayerPolygon
    startNode = do
      p <- find (\pin -> pin.label == outputPinName) (Map.findWithDefault [] root.name declaredPins)
      pinLayerPolygon lm p

    -- Every declared pin's connectivity-graph node, reverse-indexed to
    -- the (instance label, Pin) it came from.
    pinNode :: Map.Map LayerPolygon (String, Pin)
    pinNode = Map.fromList
      [ (lp, (instLbl, p))
      | (instLbl, ps) <- Map.toList declaredPins
      , p             <- ps
      , Just lp        <- [pinLayerPolygon lm p]
      ]

    -- Only ever applied to nodes from 'netPins' below, filtered to
    -- exactly the keys of 'pinNode', so the lookup always succeeds.
    pinId :: LayerPolygon -> (String, String)
    pinId n = (instLbl, p.label) where (instLbl, p) = pinNode Map.! n

    siblingNodes :: String -> [LayerPolygon]
    siblingNodes instLbl =
      [ lp | p <- Map.findWithDefault [] instLbl declaredPins, Just lp <- [pinLayerPolygon lm p] ]

    -- The full set of connectivity-graph nodes physically touching
    -- 'start' - DFS over 'adj', same as 'connectedComponents', not
    -- stopping at pin nodes so a net touching several pins is fully traced.
    netOf :: LayerPolygon -> [LayerPolygon]
    netOf start = walk [start] Set.empty
      where
        walk [] seen = Set.toList seen
        walk (n : ns) seen
          | n `Set.member` seen = walk ns seen
          | otherwise = walk (Map.findWithDefault [] n adj ++ ns) (Set.insert n seen)

    canon :: ((String, String), (String, String)) -> ((String, String), (String, String))
    canon (a, b) | a <= b    = (a, b)
                 | otherwise = (b, a)

    -- Explores a worklist of pin-node seeds: for each unvisited seed,
    -- traces its net, records every declared-pin pair found on it, then
    -- adds every other declared pin of any component touched by the net
    -- as a further seed (never continuing past a pin node via 'adj'
    -- directly). 'siblingNodes' yields nothing for an undeclared instance.
    go :: [LayerPolygon] -> Set.Set LayerPolygon -> [((String, String), (String, String))]
       -> [((String, String), (String, String))]
    go [] _ acc = acc
    go (seedNode : rest) seen acc
      | seedNode `Set.member` seen = go rest seen acc
      | otherwise = go (newSeeds ++ rest) (Set.union seen (Set.fromList netPins)) (pairs ++ acc)
      where
        netNodes = netOf seedNode
        netPins  = filter (`Map.member` pinNode) netNodes
        -- A named pin can be drawn as several disjoint shapes sharing one
        -- pinId; excluding same-pinId pairs keeps a pin from "connecting"
        -- to itself.
        pairs    =
          [ canon (pinId a, pinId b)
          | (a : bs) <- tails netPins
          , b        <- bs
          , pinId a /= pinId b
          ]
        newSeeds =
          [ compNode
          | n                <- netPins
          , Just (instLbl, _) <- [Map.lookup n pinNode]
          , compNode         <- siblingNodes instLbl
          ]
