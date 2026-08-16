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
  , pathToPolygon
  ) where

import qualified Data.DList              as DL
import           Data.Foldable1          (Foldable1, foldlMap1')
import qualified Data.IntervalMap.Strict as IM
import qualified Data.List.NonEmpty      as NE
import           Structure               (Boundary (..), Coordinate (..),
                                           Path (..), PathKind (..))

-- | error requires Text (relude's Prelude), while the rest of this module
-- builds messages as String; this is the single conversion point.
geomError :: String -> a
geomError = error . toText

-- | A closed polygonal ring in database units - the on-layer outline a
-- Boundary or Path resolves to. 'boundaryToPolygon' and 'pathToPolygon' are
-- the only ways to build one, so every Polygon traces back to a real GDS
-- element.
newtype Polygon = Polygon (NonEmpty Coordinate)
  deriving (Show, Eq)

-- | A Boundary's Xy points already form a closed ring per the GDS spec
-- (the first and last point coincide), so they carry over unchanged.
boundaryToPolygon :: Boundary -> Polygon
boundaryToPolygon b = case nonEmpty b.coords of
  Just cs -> Polygon cs
  Nothing -> geomError "boundaryToPolygon: Boundary has no coordinates"

-- | The unit direction vector from one coordinate to another.
unitVector :: Coordinate -> Coordinate -> (Double, Double)
unitVector s e
  | len == 0  = geomError "pathToPolygon: path start and end coincide"
  | otherwise = (dx / len, dy / len)
  where
    dx  = fromIntegral (e.x - s.x)
    dy  = fromIntegral (e.y - s.y)
    len = sqrt (dx * dx + dy * dy)

-- | Rotates a vector 90 degrees counter-clockwise.
perpendicular :: (Double, Double) -> (Double, Double)
perpendicular (dx, dy) = (-dy, dx)

-- | Moves a coordinate along a direction vector by the given distance,
-- rounding back to the integer GDS grid.
offsetBy :: Coordinate -> (Double, Double) -> Double -> Coordinate
offsetBy c (dx, dy) dist = Coordinate
  { x = c.x + round (dx * dist)
  , y = c.y + round (dy * dist)
  }

-- | The four corners of the rectangle a straight, unrounded stroke of the
-- given half-width sweeps out between 's' and 'e'.
strokeRectangle :: Coordinate -> Coordinate -> Double -> NonEmpty Coordinate
strokeRectangle s e halfWidth =
  eLeft :| [eRight, sRight, sLeft, eLeft]
  where
    perp   = perpendicular (unitVector s e)
    eLeft  = offsetBy e perp halfWidth
    eRight = offsetBy e perp (-halfWidth)
    sRight = offsetBy s perp (-halfWidth)
    sLeft  = offsetBy s perp halfWidth

-- | Converts a Path into the polygon it traces out, per the geometry each
-- GDS PATHTYPE specifies:
--
--   * Flush - ends are cut off exactly at the given start\/end points.
--   * Extended - ends square off half the path width beyond each point.
--   * VariableExtended - ends square off by explicit, independently chosen
--     distances at the start and the end.
--
-- Round paths are not supported - semicircular caps can't be represented
-- exactly as a polygon, so this errors rather than silently approximating.
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
  -- Rectangles are keyed by their y-interval, but distinct rectangles can
  -- share an identical y-interval while open at the same time, so each key
  -- holds every currently-open rectangle with that interval.
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
