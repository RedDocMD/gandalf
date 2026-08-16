module Main (main) where

import qualified Data.Set          as Set
import           Test.Tasty
import           Test.Tasty.HUnit

import           Geom              (Rectangle (..), RectangleBounded (..),
                                     boundIntersections)
import           Structure         (Coordinate (..))

data Box = Box
  { label   :: String
  , boxRect :: Rectangle
  }
  deriving Eq

instance RectangleBounded Box where
  boundingRect = boxRect

-- | Builds a Box from its bounds (minX, minY, maxX, maxY) rather than the
-- top-left/width/height representation Rectangle itself uses, since the
-- latter measures height downward from a top-left corner whose y is the
-- *maximum* y (matching the Polygon instance's convention).
box :: String -> Int -> Int -> Int -> Int -> Box
box l minX minY maxX maxY =
  Box l (Rectangle (Coordinate minX maxY) (maxX - minX) (maxY - minY))

-- | boundIntersections doesn't promise pair order or direction; normalize to
-- an order-independent set of label pairs so assertions aren't brittle to
-- the sweep's internal ordering.
intersectionLabels :: [(Box, Box)] -> Set.Set (String, String)
intersectionLabels = Set.fromList . map toPair
  where
    toPair (a, b)
      | label a <= label b = (label a, label b)
      | otherwise           = (label b, label a)

assertIntersections :: [Box] -> [(String, String)] -> Assertion
assertIntersections boxes expected =
  intersectionLabels (boundIntersections boxes) @?= Set.fromList expected

main :: IO ()
main = defaultMain $ testGroup "Geom.boundIntersections"
  [ testCase "fully overlapping (identical rectangles)" $
      assertIntersections
        [box "a" 0 0 10 10, box "b" 0 0 10 10]
        [("a", "b")]

  , testCase "nested (one fully contains the other)" $
      assertIntersections
        [box "outer" 0 0 10 10, box "inner" 2 2 8 8]
        [("inner", "outer")]

  , testCase "touching on x axis only" $
      assertIntersections
        [box "a" 0 0 5 5, box "b" 5 0 10 5]
        [("a", "b")]

  , testCase "touching on y axis only" $
      assertIntersections
        [box "a" 0 0 5 5, box "b" 0 5 5 10]
        [("a", "b")]

  , testCase "touching at a single corner" $
      assertIntersections
        [box "a" 0 0 5 5, box "b" 5 5 10 10]
        [("a", "b")]

  , testCase "separated in x (no intersection)" $
      assertIntersections
        [box "a" 0 0 5 5, box "b" 6 0 10 5]
        []

  , testCase "separated in y (no intersection)" $
      assertIntersections
        [box "a" 0 0 5 5, box "b" 0 6 5 10]
        []

  , testCase "ordinary partial overlap" $
      assertIntersections
        [box "a" 0 0 5 5, box "b" 3 3 8 8]
        [("a", "b")]

  , testCase "chain of three: A-B and B-C intersect, A-C do not" $
      assertIntersections
        [box "a" 0 0 5 5, box "b" 4 0 9 5, box "c" 8 0 13 5]
        [("a", "b"), ("b", "c")]
  ]
