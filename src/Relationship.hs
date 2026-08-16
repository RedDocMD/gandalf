{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}

-- | Relates a Cell's geometry to the Cells it comes from: for every layer,
-- the Polygons that live on it - including those pulled in transitively
-- through SREFs - each labeled with the Cell it was directly defined in.
-- Also relates named pin layers (per a LayerMap) to the text-labeled
-- Polygons that are their pins, and derives a physical-overlap
-- connectivity graph from a LayerMap's direct/cross-layer connections.
module Relationship
  ( LabeledPolygon (..)
  , layerPolygons
  , srefTransform
  , Pin (..)
  , pins
  , LayerPolygon (..)
  , connectivity
  ) where

import           Data.List        (isSuffixOf)
import qualified Data.Map.Lazy    as MapLazy
import qualified Data.Map.Strict  as Map
import           Geom             (Polygon, Rectangle (..),
                                    RectangleBounded (..), Transform (..),
                                    boundaryToPolygon, pathToPolygon,
                                    polygonIntersection, transformCoordinate,
                                    transformPolygon)
import           LayerMap         (CrossConnection (..),
                                    DirectConnection (..), LayerEntry (..),
                                    LayerMap (..))
import           Parse            (GdsStransFlags (GdsStransFlags))
import           Structure        (Boundary (..), Cell (..), CellRef (..),
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

-- | Whether two Polygons truly (non-zero-area) overlap.
overlaps :: Polygon -> Polygon -> Bool
overlaps p q = boundsOverlap p q && isJust (polygonIntersection p q)

-- | Every pair of Polygons found to be electrically connected within a
-- Cell, per a LayerMap's "direct_connections" (Polygons on the very same
-- named layer that physically overlap each other) and "cross_connections"
-- (a named layer A's Polygons bridged to a named layer B's Polygons by a
-- shared overlap with some via layer C's Polygons), returned as a classic
-- adjacency list: for every connected Polygon, every other Polygon it was
-- found to touch.
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
