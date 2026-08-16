{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE TupleSections         #-}

module Geom (Polygon, RectangleBounded (..), Rectangle, boundIntersections) where

import qualified Data.DList                 as DL
import           Data.Foldable1             (Foldable1, foldlMap1')
import qualified Data.IntervalMap.Strict    as IM
import           Structure                  (Coordinate (..))

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
    else compare left.x left.x

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
      , kind = SweepEventEntry
      , x = rect.topLeft.x
      , ob = a
      }

boundIntersections :: (RectangleBounded a, Eq a) => [a] -> [(a, a)]
boundIntersections = runSweepLine . sort . concatMap rectangleEvents

data SweepState a = SweepState
  { events        :: [SweepEvent a]
  , openEdges     :: IM.IntervalMap Int a
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

sweepStep :: SweepEvent a -> State (SweepState a) ()
sweepStep ev = do
  let evk = IM.ClosedInterval ev.yMin ev.yMax
  st <- get
  case ev.kind of
    SweepEventEntry -> do
      let newIntersections = DL.fromList $ map (ev.ob,) $ IM.elems $ IM.intersecting st.openEdges evk
          intersections' = st.intersections <> newIntersections
          openEdges' = IM.insert evk ev.ob st.openEdges
      put st { intersections = intersections', openEdges = openEdges' }
    SweepEventExit -> do
      let openEdges' = IM.delete evk st.openEdges
      put st { openEdges = openEdges' }

runSweepLine :: [SweepEvent a] -> [(a, a)]
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
