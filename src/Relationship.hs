{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}

-- | Relates a Cell's geometry to the Cells it comes from: for every layer,
-- the Polygons that live on it - including those pulled in transitively
-- through SREFs - each labeled with the Cell it was directly defined in.
-- Also relates named pin layers (per a LayerMap) to the text-labeled
-- Polygons that are their pins.
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

-- | A Polygon paired with the name of the Cell it was directly defined in:
-- the root Cell passed to 'layerPolygons' if it's one of that Cell's own
-- Boundary/Path elements, or the name of whichever Cell it was pulled in
-- from transitively through one or more levels of SREF. An SREF's own
-- 'name' field is exactly the name of the Cell it references, so this
-- label is stable regardless of how many levels of nesting separate the
-- element from the root - it is never rewritten as it's carried upward
-- through enclosing SREFs.
data LabeledPolygon = LabeledPolygon
  { polygon :: !Polygon
  , parent  :: !String
  } deriving (Show, Eq, Ord)

-- | Every Boundary/Path element reachable from a Cell - including
-- transitively, through SREFs - grouped by Layer and expressed in the
-- Cell's own coordinate system, with each SREF's translation/rotation/
-- mirroring applied so referenced geometry lands in the right place
-- relative to the Cell that (directly or indirectly) references it. Each
-- element is labeled per 'LabeledPolygon'.
--
-- 'cells' must contain every Cell transitively referenced from 'root'
-- (e.g. the full contents of a parsed library) - an SREF naming a Cell
-- absent from 'cells' is silently treated as contributing no geometry.
--
-- Built by knot-tying: each Cell's grouped-by-layer polygons look up the
-- memoized polygons of the Cells it references from the very map being
-- constructed, so laziness resolves the recursion without an explicit
-- topological sort (this would only loop given a cyclic SREF chain, which
-- GDS doesn't permit). This must be built with Data.Map.Lazy, not
-- .Strict: Strict's fromList forces each value into WHNF as it inserts,
-- which demands the not-yet-finished map's spine before the knot can
-- close and throws <<loop>> even for a plain DAG of references.
layerPolygons :: [Cell] -> Cell -> Map.Map Layer [LabeledPolygon]
layerPolygons cells root = perCell MapLazy.! root.name
  where
    perCell :: MapLazy.Map String (Map.Map Layer [LabeledPolygon])
    perCell = MapLazy.fromList [ (c.name, cellLayerPolygons c) | c <- cells ]

    -- Each LabeledPolygon's fields are strict (see 'LabeledPolygon'), so
    -- grouping them here with Data.Map.Strict's fromListWith never lets a
    -- chain of unevaluated polygons/labels build up behind a layer's list.
    cellLayerPolygons :: Cell -> Map.Map Layer [LabeledPolygon]
    cellLayerPolygons c = Map.fromListWith (++) $
      [ (b.layer, [LabeledPolygon (boundaryToPolygon b) c.name]) | b <- c.boundary ]
      ++ [ (p.layer, [LabeledPolygon (pathToPolygon p) c.name]) | p <- c.path ]
      ++ [ (lyr, map (transformLabeled (srefTransform ref)) lps)
         | ref <- c.cellRef
         , (lyr, lps) <- Map.toList (MapLazy.findWithDefault Map.empty ref.name perCell)
         ]

-- | Record update syntax is avoided here (in favour of matching/rebuilding
-- via the constructor) since Pin's own 'polygon' field would otherwise
-- make 'lp { polygon = ... }' an ambiguous field update now that two
-- record types in this module share that field name.
transformLabeled :: Transform -> LabeledPolygon -> LabeledPolygon
transformLabeled t (LabeledPolygon p prnt) = LabeledPolygon (transformPolygon t p) prnt

-- | The placement Geom.Transform an SREF's own translation/rotation/
-- mirroring describes, per GDS defaults: no STRANS record means no
-- reflection, and no ANGLE record means no rotation.
srefTransform :: CellRef -> Transform
srefTransform ref = Transform
  { mirrorX  = maybe False stransMirrorX ref.translation
  , angleDeg = fromMaybe 0 ref.angle
  , offset   = ref.coord
  }
  where
    stransMirrorX (GdsStransFlags mx _ _) = mx

-- | A named pin: the Polygon a net-name text label sits inside, on some
-- named ".pin" layer. Both the Polygon (via its 'LabeledPolygon', which
-- also carries the Cell it came from) and the Layer it lives on are kept
-- on the record itself, so a Pin can be traced in either direction -
-- to its Polygon, or to its Layer - without a separate lookup structure
-- (Polygon has no Ord instance to key a reverse map by).
data Pin = Pin
  { label   :: !String
  , polygon :: !LabeledPolygon
  , layer   :: !Layer
  } deriving (Show, Eq)

-- | Every pin found on a Cell: for every LayerMap entry whose name ends in
-- ".pin", every Polygon on that exact (index, datatype) Layer (via
-- 'layerPolygons') that encloses the anchor point of some text label
-- sharing that layer's GDS layer *number* - the pin/label convention this
-- follows (e.g. sky130's "li1.pin", layer 67 datatype 16, labeled by text
-- on layer 67 with some other, label-specific datatype) ties a pin's
-- geometry and its net name together purely by GDS layer number, ignoring
-- datatype/texttype on both sides. "Encloses" is tested against the
-- Polygon's bounding rectangle (see 'containsPoint') since pin shapes are
-- rectangles by PDK convention.
--
-- A pin Polygon with no enclosed label, or a label enclosed by no pin
-- Polygon, contributes nothing - only a matched (Polygon, label) pair is a
-- meaningful net connection point.
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
-- reachable from a Cell - including transitively, through SREFs - with
-- each SREF's placement applied to the anchor point so it lands in the
-- Cell's own coordinate system. Built by the same knot-tying technique as
-- 'layerPolygons' - see its comment for why Data.Map.Lazy is required.
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

-- | Whether a Coordinate falls within (or on the border of) a Rectangle -
-- topLeft.y is the rectangle's *maximum* y, per
-- Geom.RectangleBounded's convention.
containsPoint :: Rectangle -> Coordinate -> Bool
containsPoint r c =
  c.x >= r.topLeft.x && c.x <= r.topLeft.x + r.width &&
  c.y <= r.topLeft.y && c.y >= r.topLeft.y - r.height

-- | A Polygon identified, for connectivity purposes, by the named layer
-- it was gathered from - a LayerMap.LayerEntry name's dot-separated
-- prefix (e.g. "poly"), valid across every datatype under that name; see
-- 'connectivity'.
data LayerPolygon = LayerPolygon
  { layerName :: !String
  , labeled   :: !LabeledPolygon
  } deriving (Show, Eq, Ord)

-- | Every Polygon on a Cell whose LayerEntry name has the given
-- dot-separated prefix, e.g. "poly" gathers "poly.drawing", "poly.gate",
-- "poly.res", ... together, regardless of purpose/datatype - see
-- 'connectivity' for why direct/cross connections are matched this
-- broadly.
namedLayerPolygons :: Map.Map Layer [LabeledPolygon] -> LayerMap -> String -> [LayerPolygon]
namedLayerPolygons grouped lm nm =
  [ LayerPolygon nm lp
  | e  <- lm.layers
  , takeWhile (/= '.') e.name == nm
  , lp <- Map.findWithDefault [] (Layer e.layer e.datatype) grouped
  ]

-- | Whether two Polygons' bounding rectangles overlap (a touch counts) -
-- the precondition 'Geom.polygonIntersection' requires of its inputs,
-- since it doesn't check this itself.
boundsOverlap :: Polygon -> Polygon -> Bool
boundsOverlap p q =
  r1.topLeft.x <= r2.topLeft.x + r2.width && r2.topLeft.x <= r1.topLeft.x + r1.width &&
  r1.topLeft.y >= r2.topLeft.y - r2.height && r2.topLeft.y >= r1.topLeft.y - r1.height
  where
    r1 = boundingRect p
    r2 = boundingRect q

-- | Whether two Polygons are electrically touching: a true (non-zero-area)
-- overlap, per 'Geom.polygonIntersection', or plain edge-to-edge abutment -
-- sharing a boundary segment of positive length, per 'Geom.edgeTouches'.
-- Two Polygons meeting only at a single corner point count as neither, and
-- so aren't touching.
overlaps :: Polygon -> Polygon -> Bool
overlaps p q = boundsOverlap p q && (isJust (polygonIntersection p q) || edgeTouches p q)

-- | Every pair of Polygons found to be electrically connected within a
-- Cell, per a LayerMap's "direct_connections" (Polygons on the very same
-- named layer that physically touch or overlap each other, per 'overlaps')
-- and "cross_connections" (a named layer A's Polygons bridged to a named
-- layer B's Polygons by a shared touch/overlap with some via layer C's
-- Polygons), returned as a classic adjacency list: for every connected
-- Polygon, every other Polygon it was found to touch.
--
-- A "layer name" here, as it appears in the LayerMap's connection lists
-- (e.g. "poly" or "li1"), matches every LayerEntry whose own name has
-- that as its dot-separated prefix - see 'namedLayerPolygons'. GDS shares
-- one layer number across every purpose of a named layer but uses a
-- different datatype per purpose, and direct/cross connections are
-- specified at the layer-name level, not any one specific purpose.
--
-- For a cross connection A-B via C: for every A Polygon overlapping some
-- C Polygon, and every B Polygon also overlapping that same C Polygon,
-- this records an A-B edge - never A-C or B-C, since C only bridges the
-- two; it isn't itself part of either net's own shape.
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

-- | Every LayerPolygon named as a node of a 'connectivity' adjacency list
-- (the map's keys and, symmetrically, every Polygon appearing in one of
-- its edge lists), labeled with the id of its connected component - the
-- set of nodes reachable from one another by zero or more edges, treated
-- as undirected since 'connectivity' already records both directions of
-- every edge it finds. Component ids are assigned in the order their
-- component's first node is encountered while walking the map's keys, so
-- they carry no meaning beyond distinguishing one component from another.
--
-- Explores each unvisited node's component by DFS, following edges via
-- the adjacency list directly rather than consulting the map's keys
-- again - so a Polygon that only ever appears in an edge list (never as a
-- key with outgoing edges of its own) is still assigned a component, via
-- 'Map.findWithDefault' defaulting its own edge list to empty.
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

-- | Every Pin found on a Cell's own directly-defined geometry - not
-- transitively through its SREFs - by restricting 'pins' to just that
-- one Cell's own universe: an SREF naming any other Cell then
-- contributes no geometry, per 'layerPolygons''s documented fallback for
-- a Cell missing from its "cells" argument.
localPins :: LayerMap -> Cell -> [Pin]
localPins lm c = pins lm [c] c

-- | A human-readable label for one specific SREF placement: the
-- referenced Cell's name, plus its placement coordinate and (if set) its
-- rotation/mirroring - e.g. "LEAF@(100,200)" or
-- "LEAF@(100,200),rot=90.0,mirrored". Two SREFs to the very same Cell get
-- two different labels as long as they're placed differently, which
-- 'pinsByInstance' relies on to keep separate instantiations from being
-- merged together under one shared Cell name.
instanceLabel :: CellRef -> String
instanceLabel ref =
  ref.name ++ "@(" ++ show ref.coord.x ++ "," ++ show ref.coord.y ++ ")" ++ rot ++ mirror
  where
    rot = case ref.angle of
      Just a | a /= 0 -> ",rot=" ++ show a
      _               -> ""
    mirror = if maybe False (\s -> s.gdsMirrorX) ref.translation then ",mirrored" else ""

-- | Record update syntax is avoided here for the same reason as
-- 'transformLabeled' - Pin's own 'polygon' field would make a plain
-- record update ambiguous.
transformPin :: Transform -> Pin -> Pin
transformPin t (Pin lbl (LabeledPolygon p prnt) lyr) =
  Pin lbl (LabeledPolygon (transformPolygon t p) prnt) lyr

-- | Every Pin reachable from a Cell - including transitively, through
-- SREFs - grouped by the specific instance it was directly defined on:
-- the root Cell's own name for a Pin defined directly on it, or an
-- SREF-placement-qualified label (see 'instanceLabel') for a Pin pulled
-- in through some SREF.
--
-- Unlike 'pins' (whose LabeledPolygon.parent is deliberately just the
-- bare owning Cell name, stable no matter how many SREFs it's reached
-- through), this keeps multiple instantiations of the very same Cell -
-- whether from two different SREFs to it, or the same Cell reached via
-- two different parent paths - as separate groups, since each is a
-- physically distinct placement with its own Pins.
pinsByInstance :: LayerMap -> [Cell] -> Cell -> Map.Map String [Pin]
pinsByInstance lm cells root =
  Map.fromListWith (++) [ (lbl, [p]) | (lbl, p) <- walk root.name root ]
  where
    cellByName :: Map.Map String Cell
    cellByName = Map.fromList [ (c.name, c) | c <- cells ]

    -- Every Pin reachable from Cell c (including transitively through its
    -- own SREFs), expressed in c's own local coordinate system, each
    -- tagged with the instance label of whichever Cell - c itself, or an
    -- SREF-placed descendant - it was directly defined on. A descendant's
    -- Pins are transformed once per level as they bubble back up through
    -- each enclosing SREF - the same telescoping composition
    -- 'layerPolygons' relies on - but here the label attached at the
    -- point of origin is carried up unchanged, rather than collapsing to
    -- a bare Cell name.
    walk :: String -> Cell -> [(String, Pin)]
    walk selfLabel c =
      [ (selfLabel, p) | p <- localPins lm c ]
      ++ [ (lbl, transformPin (srefTransform ref) p)
         | ref <- c.cellRef
         , Just child <- [Map.lookup ref.name cellByName]
         , (lbl, p) <- walk (instanceLabel ref) child
         ]

-- | Every instance label reachable from a Cell (per 'pinsByInstance''s
-- grouping) paired with the name of the Cell it's an instance of - the
-- root Cell's own name for itself (its "type" is trivially itself), or an
-- SREF's own 'name' field (the Cell it references) for every
-- transitively reached instance. This is 'pinsByInstance''s own instance
-- labeling, computed independently of any Pins, so a 'netlist' lookup can
-- tell what a given instance label was placed *from* without re-deriving
-- it from the label string.
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
-- LayerMap entry with exactly this (layer, datatype) pair - the same
-- convention 'namedLayerPolygons' matches connectivity layer names
-- against.
layerPrefixName :: LayerMap -> Layer -> Maybe String
layerPrefixName lm lyr = listToMaybe
  [ takeWhile (/= '.') e.name | e <- lm.layers, e.layer == lyr.index, e.datatype == lyr.kind ]

-- | The 'connectivity' graph node (per its adjacency list's keys) a Pin's
-- own Polygon corresponds to, if its Layer belongs to some named layer -
-- 'Nothing' for a Pin whose Layer isn't named at all in the LayerMap
-- (shouldn't normally happen, since every ".pin" layer 'pins' matches
-- against is itself a named LayerMap entry, sharing its GDS layer number
-- with some other purpose under the same name).
pinLayerPolygon :: LayerMap -> Pin -> Maybe LayerPolygon
pinLayerPolygon lm p = (\nm -> LayerPolygon nm p.polygon) <$> layerPrefixName lm p.layer

-- | Every ((component, pin), (component, pin)) connection reachable from
-- a named pin on a Cell's own definition - "reachable" meaning: starting
-- from that pin's own physically connected net (a maximal set of
-- touching Polygons, per 'connectivity'), then, for every other declared
-- pin also touching that net, fanning out to that pin's own component's
-- *other* declared pins - without ever tracing a component's own internal
-- wiring - to explore whatever nets *they* touch, and so on transitively.
-- Every pair of declared pins sharing a single net becomes one
-- connection; a net touching no declared pin, or only one, contributes
-- none.
--
-- "Declared" means: the pin's owning instance's type has an entry in the
-- given ComponentList, and the pin's own label is named among that
-- entry's own pins - see 'declaredPins'. A Pin found geometrically (via
-- 'pinsByInstance') but absent from its component's declared pin list -
-- e.g. a power/ground pin like VPWR/VGND that a components-file entry
-- doesn't bother naming - never counts as a netlist node at all: not as
-- a connection endpoint, and not as somewhere to fan out from. This is
-- why a components file is needed at all, on top of the LayerMap: it's
-- what tells the search which SREF instances are opaque "components",
-- and which of their pins are worth reporting, as opposed to plain
-- unrecognized geometry that only contributes to net shapes.
--
-- Errors if 'outputPinName' isn't declared among root's own pins (root
-- must itself have a ComponentList entry, matching its own Cell name -
-- the same convention every other placed component follows).
netlist :: LayerMap -> ComponentList -> [Cell] -> Cell -> String -> Set.Set ((String, String), (String, String))
netlist lm compList cells root outputPinName = case startNode of
  Nothing   -> error (toText ("netlist: no such declared pin " ++ show outputPinName ++ " on cell " ++ show root.name))
  Just seed -> Set.fromList (go [seed] Set.empty [])
  where
    adj   = connectivity lm cells root
    types = instanceTypes cells root

    -- Every geometrically-found Pin (per 'pinsByInstance'), restricted to
    -- just those actually named in their owning instance's own
    -- components-file entry - a Pin whose instance has no ComponentList
    -- entry at all (an unrecognized SREF) is dropped entirely, since
    -- 'Map.findWithDefault' on a missing key already yields no pins.
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

    -- Every declared pin's own connectivity-graph node, reverse-indexed
    -- to the (instance label, Pin) it came from.
    pinNode :: Map.Map LayerPolygon (String, Pin)
    pinNode = Map.fromList
      [ (lp, (instLbl, p))
      | (instLbl, ps) <- Map.toList declaredPins
      , p             <- ps
      , Just lp        <- [pinLayerPolygon lm p]
      ]

    -- pinId is only ever applied to nodes drawn from 'netPins' below,
    -- which are filtered to exactly the keys of 'pinNode' - so the lookup
    -- always succeeds.
    pinId :: LayerPolygon -> (String, String)
    pinId n = (instLbl, p.label) where (instLbl, p) = pinNode Map.! n

    siblingNodes :: String -> [LayerPolygon]
    siblingNodes instLbl =
      [ lp | p <- Map.findWithDefault [] instLbl declaredPins, Just lp <- [pinLayerPolygon lm p] ]

    -- The full set of connectivity-graph nodes physically touching
    -- 'start', found the same way 'connectedComponents' explores one
    -- component - by DFS over 'adj', not stopping at pin nodes, so a net
    -- touching several pins is fully traced.
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

    -- Explores a worklist of pin nodes to seed net traces from: for each
    -- unvisited seed, traces its whole net, records every pair of
    -- declared pins found on that net, then - per 'netlist''s own
    -- documentation - adds every *other*, not-yet-searched declared pin
    -- of any component touched by the net as a further seed (an
    -- undeclared or unrecognized instance's pins are never among
    -- 'declaredPins' in the first place, so 'siblingNodes' naturally
    -- yields nothing for one), rather than ever continuing past a pin
    -- node via 'adj' directly.
    go :: [LayerPolygon] -> Set.Set LayerPolygon -> [((String, String), (String, String))]
       -> [((String, String), (String, String))]
    go [] _ acc = acc
    go (seedNode : rest) seen acc
      | seedNode `Set.member` seen = go rest seen acc
      | otherwise = go (newSeeds ++ rest) (Set.union seen (Set.fromList netPins)) (pairs ++ acc)
      where
        netNodes = netOf seedNode
        netPins  = filter (`Map.member` pinNode) netNodes
        -- A single named pin can be drawn as several disjoint shapes (so
        -- more than one node in 'netPins' can share the very same
        -- pinId) - excluding same-pinId pairs keeps a pin from ever
        -- appearing "connected" to itself.
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
