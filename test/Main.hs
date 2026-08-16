module Main (main) where

import qualified Data.Set          as Set
import           Test.Tasty
import           Test.Tasty.HUnit

import           Geom              (Polygon, Rectangle (..),
                                     RectangleBounded (..), boundIntersections,
                                     boundaryToPolygon, polygonIntersection,
                                     samePolygon)
import           Structure         (Boundary (..), Coordinate (..),
                                     Layer (..))

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

-- | A single-use Layer, since polygonIntersection doesn't care what layer
-- its inputs came from.
dummyLayer :: Layer
dummyLayer = Layer { index = 0, kind = 0 }

-- | Builds a rectilinear Polygon from an explicit vertex list (the ring's
-- closing point is appended automatically).
polyFrom :: [(Int, Int)] -> Polygon
polyFrom []             = error "polyFrom: empty vertex list"
polyFrom pts@((x0, y0) : _) =
  boundaryToPolygon Boundary { layer = dummyLayer, coords = cs ++ [Coordinate { x = x0, y = y0 }] }
  where
    cs = map (\(px, py) -> Coordinate { x = px, y = py }) pts

rectPoly :: Int -> Int -> Int -> Int -> Polygon
rectPoly minX minY maxX maxY =
  polyFrom [(minX, minY), (maxX, minY), (maxX, maxY), (minX, maxY)]

-- | Order-independent comparison of a list of intersection components
-- against the expected shapes (see 'samePolygon' for why exact vertex
-- order/starting-point isn't part of the comparison).
sameComponents :: [Polygon] -> [Polygon] -> Bool
sameComponents as bs = length as == length bs && all (\a -> any (samePolygon a) bs) as

assertNoIntersection :: Polygon -> Polygon -> Assertion
assertNoIntersection a b = polygonIntersection a b @?= Nothing

assertIntersectionIs :: Polygon -> Polygon -> [Polygon] -> Assertion
assertIntersectionIs a b expected = case polygonIntersection a b of
  Nothing     -> assertFailure "expected an intersection, got Nothing"
  Just actual -> assertBool
    ("intersection didn't match the expected shape(s): " ++ show actual)
    (sameComponents expected actual)

main :: IO ()
main = defaultMain $ testGroup "Geom"
  [ testGroup "boundIntersections"
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

  , testGroup "polygonIntersection"
    [ testCase "L-shape vs. a rectangle sitting entirely in its notch" $
        -- L covers [0,10]x[0,10] minus the notch [4,10]x[4,10].
        assertNoIntersection
          (polyFrom [(0, 0), (10, 0), (10, 4), (4, 4), (4, 10), (0, 10)])
          (rectPoly 5 5 9 9)

    , testCase "simple rectangle overlap" $
        assertIntersectionIs
          (rectPoly 0 0 10 10)
          (rectPoly 5 5 15 15)
          [rectPoly 5 5 10 10]

    , testCase "one polygon fully inside another" $
        assertIntersectionIs
          (rectPoly 0 0 20 20)
          (rectPoly 5 5 15 15)
          [rectPoly 5 5 15 15]

    , testCase "touching only on a shared edge (zero area)" $
        assertNoIntersection
          (rectPoly 0 0 5 5)
          (rectPoly 5 0 10 5)

    , testCase "U-shape vs. a bar spanning its notch splits into two pieces" $
        -- U covers the left/right legs [0,4]x[0,10] and [8,12]x[0,10], plus
        -- the base [0,12]x[0,4] - the notch [4,8]x[4,10] is empty.
        assertIntersectionIs
          (polyFrom [(0, 0), (12, 0), (12, 10), (8, 10), (8, 4), (4, 4), (4, 10), (0, 10)])
          (rectPoly 0 6 12 8)
          [rectPoly 0 6 4 8, rectPoly 8 6 12 8]

    , testCase "step-shaped partial overlap exercises the seam-fringe fix" $
        -- A steps down from y=10 to y=6 at x=5; B is a plain full-width
        -- band. At x=5, A's coverage shrinks while B's doesn't, so the
        -- correct seam is a small fringe edge, not a full-segment cancel.
        assertIntersectionIs
          (polyFrom [(0, 0), (10, 0), (10, 6), (5, 6), (5, 10), (0, 10)])
          (rectPoly 0 3 10 10)
          [polyFrom [(0, 3), (10, 3), (10, 6), (5, 6), (5, 10), (0, 10)]]

    -- No test case exercises the "intersection encloses a hole" geomError
    -- path: a Mayer-Vietoris/Euler-characteristic argument (components -
    -- holes = 2 - components(A u B), and A u B is always connected when
    -- A n B is non-empty) shows a *single-component* hole is topologically
    -- impossible from two simple polygons - it would need a second,
    -- separate component to "spend" on satisfying that count, which in
    -- turn needs its own delicate, non-generic construction. An extensive
    -- search (thousands of randomized and hand-derived rectilinear
    -- candidates) turned up none, so the branch is believed unreachable in
    -- practice for real GDS shapes and is exercised only by code review,
    -- not a constructed example.
    ]
  ]
