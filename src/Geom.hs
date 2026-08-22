{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE TupleSections         #-}

module Geom
  ( Polygon
  , RectangleBounded (..)
  , Rectangle (..)
  , boundIntersections
  , boundaryToPolygon
  , edgeTouches
  , pathToPolygon
  , polygonIntersection
  , polygonVertices
  , samePolygon
  , Transform (..)
  , transformCoordinate
  , transformPolygon
  ) where

import qualified Data.DList              as DL
import           Data.Foldable1          (Foldable1, foldlMap1')
import qualified Data.IntervalMap.Strict as IM
import qualified Data.List.NonEmpty      as NE
import qualified Data.Map.Strict         as Map
import qualified Data.Set                as Set
import           Structure               (Boundary (..), Coordinate (..),
                                           Path (..), PathKind (..))

-- | relude's error wants Text; single conversion point for this module.
geomError :: String -> a
geomError = error . toText

-- | A closed polygonal ring in database units - the on-layer outline of a
-- Boundary or Path. Only 'boundaryToPolygon' and 'pathToPolygon' build one.
newtype Polygon = Polygon (NonEmpty Coordinate)
  deriving (Show, Eq, Ord)

-- | The ring of coordinates a Polygon traces, closing back to its start.
polygonVertices :: Polygon -> [Coordinate]
polygonVertices (Polygon cs) = toList cs

-- | A Boundary's Xy points already form a closed ring per the GDS spec.
boundaryToPolygon :: Boundary -> Polygon
boundaryToPolygon b = case nonEmpty b.coords of
  Just cs -> Polygon cs
  Nothing -> geomError "boundaryToPolygon: Boundary has no coordinates"

-- | Unit direction vector from one coordinate to another.
unitVector :: Coordinate -> Coordinate -> (Double, Double)
unitVector s e
  | len == 0  = geomError "pathToPolygon: path start and end coincide"
  | otherwise = (dx / len, dy / len)
  where
    dx  = fromIntegral (e.x - s.x)
    dy  = fromIntegral (e.y - s.y)
    len = sqrt (dx * dx + dy * dy)

perpendicular :: (Double, Double) -> (Double, Double)
perpendicular (dx, dy) = (-dy, dx)

-- | Moves a coordinate along a direction vector by a distance, rounding
-- back to the integer GDS grid.
offsetBy :: Coordinate -> (Double, Double) -> Double -> Coordinate
offsetBy c (dx, dy) dist = Coordinate
  { x = c.x + round (dx * dist)
  , y = c.y + round (dy * dist)
  }

-- | The four corners of the rectangle an unrounded stroke of the given
-- half-width sweeps out between 's' and 'e'.
strokeRectangle :: Coordinate -> Coordinate -> Double -> NonEmpty Coordinate
strokeRectangle s e halfWidth =
  eLeft :| [eRight, sRight, sLeft, eLeft]
  where
    perp   = perpendicular (unitVector s e)
    eLeft  = offsetBy e perp halfWidth
    eRight = offsetBy e perp (-halfWidth)
    sRight = offsetBy s perp (-halfWidth)
    sLeft  = offsetBy s perp halfWidth

-- | Converts a Path to the polygon it traces, per its GDS PATHTYPE (Flush,
-- Extended, VariableExtended). Errors on Round: semicircular caps can't be
-- represented exactly as a polygon.
pathToPolygon :: Path -> Polygon
pathToPolygon p = Polygon $ case p.kind of
  Flush -> strokeRectangle p.start p.end halfWidth
  Extended -> strokeRectangle
    (offsetBy p.start dir (-halfWidth))
    (offsetBy p.end dir halfWidth)
    halfWidth
  VariableExtended { start = bgnExtn, end = endExtn } -> strokeRectangle
    (offsetBy p.start dir (-fromIntegral bgnExtn))
    (offsetBy p.end dir (fromIntegral endExtn))
    halfWidth
  Round -> geomError "pathToPolygon: Round path caps are not supported"
  where
    halfWidth = fromIntegral p.width / 2
    dir       = unitVector p.start p.end

-- | An SREF's placement: x-axis reflection (if set), then counter-clockwise
-- rotation, then translation - the STRANS\/ANGLE\/SREF composition order.
-- Magnification isn't parsed off CellRef, so it isn't applied here.
data Transform = Transform
  { mirrorX  :: Bool
  , angleDeg :: Double
  , offset   :: Coordinate
  }

transformPolygon :: Transform -> Polygon -> Polygon
transformPolygon t (Polygon cs) = Polygon (fmap (transformCoordinate t) cs)

transformCoordinate :: Transform -> Coordinate -> Coordinate
transformCoordinate t c = Coordinate
  { x = round (rx + fromIntegral t.offset.x)
  , y = round (ry + fromIntegral t.offset.y)
  }
  where
    theta :: Double
    theta = t.angleDeg * pi / 180
    mx, my :: Double
    mx = fromIntegral c.x
    my = if t.mirrorX then negate (fromIntegral c.y) else fromIntegral c.y
    rx = mx * cos theta - my * sin theta
    ry = mx * sin theta + my * cos theta

data Rectangle = Rectangle
  { topLeft :: Coordinate
  , width   :: Int
  , height  :: Int
  }
  deriving (Show, Eq)

class RectangleBounded a where
  boundingRect :: a -> Rectangle

foldable1MinMax :: (Foldable1 f, Ord a) => f a -> (a, a)
foldable1MinMax = foldlMap1' (\x -> (x, x)) (\(minA, maxA) x -> (min minA x, max maxA x))

instance RectangleBounded Polygon where
  boundingRect (Polygon coords) = Rectangle
    { topLeft = Coordinate
        { x = minX
        , y = maxY
        }
    , width = maxX - minX
    , height = maxY - minY
    }
    where
      (minX, maxX) = foldable1MinMax $ fmap (\c -> c.x) coords
      (minY, maxY) = foldable1MinMax $ fmap (\c -> c.y) coords

data SweepEventKind = SweepEventEntry | SweepEventExit deriving (Show, Ord, Eq)

data SweepEvent a = SweepEvent
  { yMin :: Int
  , yMax :: Int
  , kind :: SweepEventKind
  , x    :: Int
  , ob   :: a
  }
  deriving (Show, Eq)

instance Eq a => Ord (SweepEvent a) where
  compare left right =
    if left.x == right.x
    then compare left.kind right.kind
    else compare left.x right.x

rectangleEvents :: RectangleBounded a => a -> [SweepEvent a]
rectangleEvents a = [entry, exit]
  where
    rect = boundingRect a
    entry = SweepEvent
      { yMin = rect.topLeft.y - rect.height
      , yMax = rect.topLeft.y
      , kind = SweepEventEntry
      , x = rect.topLeft.x
      , ob = a
      }
    exit = SweepEvent
      { yMin = rect.topLeft.y - rect.height
      , yMax = rect.topLeft.y
      , kind = SweepEventExit
      , x = rect.topLeft.x + rect.width
      , ob = a
      }

boundIntersections :: (RectangleBounded a, Eq a) => [a] -> [(a, a)]
boundIntersections = runSweepLine . sort . concatMap rectangleEvents

data SweepState a = SweepState
  { events        :: [SweepEvent a]
  -- Keyed by y-interval; a key holds every currently-open rectangle
  -- sharing that interval, since more than one can be open at once.
  , openEdges     :: IM.IntervalMap Int (NonEmpty a)
  , intersections :: DL.DList (a, a)
  }

nextEventToProcess :: State (SweepState a) (Maybe (SweepEvent a))
nextEventToProcess = do
  st <- get
  case st.events of
    [] -> return Nothing
    x:xs -> do
      put st { events = xs }
      return $ Just x

sweepStep :: Eq a => SweepEvent a -> State (SweepState a) ()
sweepStep ev = do
  let evk = IM.ClosedInterval ev.yMin ev.yMax
  st <- get
  case ev.kind of
    SweepEventEntry -> do
      let newIntersections = DL.fromList $ map (ev.ob,) $ concatMap toList $ IM.elems $ IM.intersecting st.openEdges evk
          intersections' = st.intersections <> newIntersections
          openEdges' = IM.insertWith (<>) evk (ev.ob :| []) st.openEdges
      put st { intersections = intersections', openEdges = openEdges' }
    SweepEventExit -> do
      let openEdges' = IM.update (nonEmpty . NE.filter (/= ev.ob)) evk st.openEdges
      put st { openEdges = openEdges' }

runSweepLine :: Eq a => [SweepEvent a] -> [(a, a)]
runSweepLine evs = evalState runSweepLineImpl initState
  where
    initState = SweepState
      { events = evs
      , openEdges = IM.empty
      , intersections = DL.empty
      }
    runSweepLineImpl = do
      ev' <- nextEventToProcess
      case ev' of
        Just ev -> do
          sweepStep ev
          runSweepLineImpl
        Nothing -> do
          st <- get
          return $ DL.toList st.intersections

-- === Rectilinear polygon intersection =====================================

-- | An (x, y) pair for the sweep below. Kept separate from 'Coordinate'
-- (no 'Ord' instance) so this algorithm doesn't impose a spatial order on
-- the public GDS-domain type; converted only at the two boundaries.
type Point = (Int, Int)

pointToCoord :: Point -> Coordinate
pointToCoord (px, py) = Coordinate { x = px, y = py }

-- | A vertical edge of a rectilinear polygon, with vYLo < vYHi.
data VerticalEdge = VerticalEdge
  { vX   :: !Int
  , vYLo :: !Int
  , vYHi :: !Int
  }
  deriving (Show, Eq)

-- | A horizontal edge of a rectilinear polygon's ring, with hXLo < hXHi.
data HorizontalEdge = HorizontalEdge
  { hY   :: !Int
  , hXLo :: !Int
  , hXHi :: !Int
  }
  deriving (Show, Eq)

-- | Splits a rectilinear polygon's ring into vertical and horizontal edges,
-- validating each is axis-aligned and non-degenerate.
polygonEdges :: Polygon -> ([VerticalEdge], [HorizontalEdge])
polygonEdges (Polygon coords) = foldMap classify (zip cs (NE.tail coords))
  where
    cs = toList coords
    classify :: (Coordinate, Coordinate) -> ([VerticalEdge], [HorizontalEdge])
    classify (a, b)
      | a.x == b.x && a.y == b.y =
          geomError "polygonIntersection: degenerate zero-length edge"
      | a.x == b.x =
          ([VerticalEdge { vX = a.x, vYLo = min a.y b.y, vYHi = max a.y b.y }], [])
      | a.y == b.y =
          ([], [HorizontalEdge { hY = a.y, hXLo = min a.x b.x, hXHi = max a.x b.x }])
      | otherwise  = geomError "polygonIntersection: non-rectilinear edge"

verticalEdges :: Polygon -> [VerticalEdge]
verticalEdges = fst . polygonEdges

horizontalEdges :: Polygon -> [HorizontalEdge]
horizontalEdges = snd . polygonEdges

-- | Whether two closed integer intervals overlap with positive length -
-- sharing only an endpoint doesn't count.
overlapsPositively :: Int -> Int -> Int -> Int -> Bool
overlapsPositively aLo aHi bLo bHi = max aLo bLo < min aHi bHi

-- | Whether two rectilinear polygons abut along a shared boundary segment
-- of positive length, as distinct from touching at a single corner.
-- Independent of 'polygonIntersection' (e.g. nesting overlaps in area
-- without sharing a boundary) - a caller wanting either checks both.
edgeTouches :: Polygon -> Polygon -> Bool
edgeTouches polyA polyB = verticalTouch || horizontalTouch
  where
    verticalTouch = or
      [ overlapsPositively a.vYLo a.vYHi b.vYLo b.vYHi
      | a <- verticalEdges polyA, b <- verticalEdges polyB, a.vX == b.vX ]
    horizontalTouch = or
      [ overlapsPositively a.hXLo a.hXHi b.hXLo b.hXHi
      | a <- horizontalEdges polyA, b <- horizontalEdges polyB, a.hY == b.hY ]

-- | Groups vertical edges by x, for slab-by-slab lookup during the sweep.
edgesByX :: [VerticalEdge] -> Map.Map Int [VerticalEdge]
edgesByX = Map.fromListWith (++) . map (\e -> (e.vX, [e]))

-- | A polygon's ray-casting parity state at the sweep's current x: 'flips'
-- is the set of y coordinates where inside/outside parity toggles;
-- 'intervals' is the same state as inside-y-ranges, from pairing up the
-- sorted flips - recomputed only when 'flips' changes.
data PolySweep = PolySweep
  { flips     :: !(Set.Set Int)
  , intervals :: ![(Int, Int)]
  }

emptySweep :: PolySweep
emptySweep = PolySweep { flips = Set.empty, intervals = [] }

toggleY :: Set.Set Int -> Int -> Set.Set Int
toggleY s y
  | Set.member y s = Set.delete y s
  | otherwise      = Set.insert y s

pairUp :: [Int] -> [(Int, Int)]
pairUp (a : b : rest) = (a, b) : pairUp rest
pairUp _              = []

applyEdgesAt :: PolySweep -> [VerticalEdge] -> PolySweep
applyEdgesAt sw []    = sw
applyEdgesAt sw edges = PolySweep { flips = flips', intervals = pairUp (Set.toAscList flips') }
  where
    flips' = foldl' (\s e -> toggleY (toggleY s e.vYLo) e.vYHi) sw.flips edges

-- | Intersects two sorted, disjoint interval lists, keeping only overlaps
-- with positive width (a zero-width touch doesn't count, see
-- 'polygonIntersection').
twoPointerIntersect :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
twoPointerIntersect = go
  where
    go [] _  = []
    go _  [] = []
    go aas@((aLo, aHi) : as) bbs@((bLo, bHi) : bs)
      | aHi < bHi = overlap ++ go as bbs
      | otherwise = overlap ++ go aas bs
      where
        lo = max aLo bLo
        hi = min aHi bHi
        overlap = [(lo, hi) | lo < hi]

-- | Subtracts sorted, disjoint interval list b from a, returning the
-- sorted, disjoint remainder.
subtractIntervals :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
subtractIntervals []  _ = []
subtractIntervals xs [] = xs
subtractIntervals aas@((aLo, aHi) : as) bbs@((bLo, bHi) : bs)
  | aHi <= bLo = (aLo, aHi) : subtractIntervals as bbs
  | bHi <= aLo = subtractIntervals aas bs
  | otherwise  = leftPart ++ rest
  where
    leftPart = [(aLo, bLo) | aLo < bLo]
    rest
      | aHi > bHi = subtractIntervals ((bHi, aHi) : as) bs
      | otherwise = subtractIntervals as bbs

insertEdge :: Point -> Point -> Map.Map Point Point -> Map.Map Point Point
insertEdge = Map.insertWith
  (\_ _ -> geomError "polygonIntersection: malformed boundary (vertex has multiple outgoing edges)")

-- | Emits the vertical boundary edges at x, from the symmetric difference
-- between the intersection's y-coverage just left of x ('prevSlab') and
-- just right ('thisSlab'). A range only on the left ends the region here
-- (a right edge, drawn upward); a range only on the right starts it (a
-- left edge, drawn downward) - the counter-clockwise convention (interior
-- on an edge's left).
addSeamEdges :: Int -> [(Int, Int)] -> [(Int, Int)] -> Map.Map Point Point -> Map.Map Point Point
addSeamEdges x prevSlab thisSlab adj0 =
  foldl' addDown (foldl' addUp adj0 ending) starting
  where
    ending   = subtractIntervals prevSlab thisSlab
    starting = subtractIntervals thisSlab prevSlab
    addUp   adj (p, q) = insertEdge (x, p) (x, q) adj
    addDown adj (p, q) = insertEdge (x, q) (x, p) adj

-- | Emits the horizontal boundary edges across slab (x, xNext): one bottom
-- (rightward) and one top (leftward) edge per inside y-interval.
addHorizontalEdges :: Int -> Int -> [(Int, Int)] -> Map.Map Point Point -> Map.Map Point Point
addHorizontalEdges x xNext slab adj0 = foldl' addPair adj0 slab
  where
    addPair adj (yLo, yHi) =
      insertEdge (xNext, yHi) (x, yHi) (insertEdge (x, yLo) (xNext, yLo) adj)

data SweepAcc = SweepAcc
  { accA         :: !PolySweep
  , accB         :: !PolySweep
  , accPrevSlab  :: ![(Int, Int)]
  , accAdjacency :: !(Map.Map Point Point)
  }

withNext :: [a] -> [(a, Maybe a)]
withNext []                 = []
withNext [x]                = [(x, Nothing)]
withNext (x : rest@(y : _)) = (x, Just y) : withNext rest

-- | Sweeps left to right across the union of both polygons' vertical-edge
-- x coordinates, building the intersection boundary's adjacency map: each
-- key is a boundary vertex, its value the next vertex (counter-clockwise).
runXSweep :: Polygon -> Polygon -> Map.Map Point Point
runXSweep polyA polyB = (foldl' step initAcc (withNext xs)).accAdjacency
  where
    edgesA = edgesByX (verticalEdges polyA)
    edgesB = edgesByX (verticalEdges polyB)
    xs = Set.toAscList (Set.union (Map.keysSet edgesA) (Map.keysSet edgesB))
    initAcc = SweepAcc
      { accA = emptySweep
      , accB = emptySweep
      , accPrevSlab = []
      , accAdjacency = Map.empty
      }
    step :: SweepAcc -> (Int, Maybe Int) -> SweepAcc
    step !acc (x, mNext) = SweepAcc
      { accA = sweepA'
      , accB = sweepB'
      , accPrevSlab = thisSlab
      , accAdjacency = adjacency'
      }
      where
        sweepA'    = applyEdgesAt acc.accA (Map.findWithDefault [] x edgesA)
        sweepB'    = applyEdgesAt acc.accB (Map.findWithDefault [] x edgesB)
        thisSlab   = twoPointerIntersect sweepA'.intervals sweepB'.intervals
        withSeam   = addSeamEdges x acc.accPrevSlab thisSlab acc.accAdjacency
        adjacency' = case mNext of
          Just xNext -> addHorizontalEdges x xNext thisSlab withSeam
          Nothing    -> withSeam

-- | Traces the boundary adjacency map into closed loops, consuming each
-- vertex exactly once. Each loop is an outer boundary (counter-clockwise,
-- by construction) or - if the intersection encloses a gap - a hole
-- (clockwise); see 'polygonIntersection'.
traceLoops :: Map.Map Point Point -> [[Point]]
traceLoops adj
  | Map.null adj = []
  | otherwise    = loop : traceLoops rest
  where
    (start, _)   = Map.findMin adj
    (loop, rest) = walk start start adj []

    walk :: Point -> Point -> Map.Map Point Point -> [Point] -> ([Point], Map.Map Point Point)
    walk start' cur adj' acc = case Map.lookup cur adj' of
      Nothing   -> geomError "polygonIntersection: boundary failed to close"
      Just next ->
        let !adj'' = Map.delete cur adj'
            !acc'  = cur : acc
        in if next == start'
             then (reverse acc', adj'')
             else walk start' next adj'' acc'

-- | Cyclically rotates a list left by n elements.
rotate :: Int -> [a] -> [a]
rotate n xs = take len (drop (n `mod` len) (cycle xs))
  where
    len = length xs

-- | Twice the signed (shoelace) area of a closed loop: positive for
-- counter-clockwise, negative for clockwise.
signedArea2x :: [Point] -> Int
signedArea2x pts = sum (zipWith cross pts (rotate 1 pts))
  where
    cross (x1, y1) (x2, y2) = x1 * y2 - x2 * y1

-- | Drops vertices in the middle of a straight run (incoming and outgoing
-- edge directions match), left behind at slabs where y-coverage doesn't
-- actually change.
simplifyLoop :: [Point] -> [Point]
simplifyLoop pts =
  [ p | (prev, p, next) <- zip3 (rotate (-1) pts) pts (rotate 1 pts)
      , direction prev p /= direction p next
  ]
  where
    direction (ax, ay) (bx, by) = (signum (bx - ax), signum (by - ay))

pointsToPolygon :: [Point] -> Polygon
pointsToPolygon []       = geomError "polygonIntersection: empty loop"
pointsToPolygon (p : ps) = Polygon (c :| (map pointToCoord ps ++ [c]))
  where
    c = pointToCoord p

-- | The true geometric intersection of two rectilinear polygons, as one
-- simple polygon per connected component of the overlap. Returns 'Nothing'
-- if they don't overlap (a zero-area touch counts as no overlap).
--
-- Precondition: the two polygons' bounding boxes are already known to
-- overlap (see 'boundIntersections'); not re-checked here.
--
-- Errors if a component of the intersection encloses a hole - not
-- representable as a single ring, and possible even though both inputs are
-- hole-free (e.g. two bracket-shaped polygons overlapping around a gap
-- neither covers).
polygonIntersection :: Polygon -> Polygon -> Maybe [Polygon]
polygonIntersection polyA polyB = case traceLoops (runXSweep polyA polyB) of
  []    -> Nothing
  loops
    | all ((> 0) . signedArea2x) loops -> Just (map (pointsToPolygon . simplifyLoop) loops)
    | otherwise -> geomError "polygonIntersection: intersection encloses a hole, not representable"

-- | True if two polygons trace the same cyclic boundary, regardless of
-- starting vertex or traversal direction.
samePolygon :: Polygon -> Polygon -> Bool
samePolygon (Polygon a) (Polygon b) =
  ringA `elem` rotations ringB || ringA `elem` rotations (reverse ringB)
  where
    ringA = NE.init a
    ringB = NE.init b
    rotations xs = take (length xs) (iterate rotateLeft1 xs)
    rotateLeft1 []       = []
    rotateLeft1 (y : ys) = ys ++ [y]
