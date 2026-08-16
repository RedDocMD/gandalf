{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE TupleSections         #-}

module Geom (Polygon, RectangleBounded (..), Rectangle (..), boundIntersections) where

import qualified Data.DList              as DL
import           Data.Foldable1          (Foldable1, foldlMap1')
import qualified Data.IntervalMap.Strict as IM
import qualified Data.List.NonEmpty      as NE
import           Structure               (Coordinate (..))

newtype Polygon = Polygon (NonEmpty Coordinate)
  deriving Show

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
