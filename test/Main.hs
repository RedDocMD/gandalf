{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}

module Main (main) where

import           Control.Exception (SomeException, evaluate, try)
import qualified Data.Map          as Map
import qualified Data.Set          as Set
import           Test.Tasty
import           Test.Tasty.HUnit

import qualified Component
import           Component         (readComponentList)
import           Geom              (Polygon, Rectangle (..),
                                    RectangleBounded (..), boundIntersections,
                                    boundaryToPolygon, polygonIntersection,
                                    samePolygon)
import           LayerMap          (CrossConnection (..),
                                    DirectConnection (..), LayerEntry (..),
                                    LayerMap (..))
import           Relationship      (LabeledPolygon (..), LayerPolygon (..),
                                    Pin (..), connectedComponents,
                                    connectivity, layerPolygons, netlist,
                                    pins, pinsByInstance)
import           Structure         (Boundary (..), Cell (..), CellRef (..),
                                    Coordinate (..), Layer (..),
                                    TextDescription (..))

data Box = Box
  { label   :: String
  , boxRect :: Rectangle
  }
  deriving Eq

instance RectangleBounded Box where
  boundingRect b = b.boxRect

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
    toPair :: (Box, Box) -> (String, String)
    toPair (a, b)
      | a.label <= b.label = (a.label, b.label)
      | otherwise           = (b.label, a.label)

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
main = defaultMain $ testGroup "gandalf" [geomTests, relationshipTests, netlistTests, componentTests]

geomTests :: TestTree
geomTests = testGroup "Geom"
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

-- === Relationship ===========================================================

coord :: Int -> Int -> Coordinate
coord cx cy = Coordinate { x = cx, y = cy }

-- | A rectangle's Boundary coordinate ring, closing back to its start
-- point per the GDS convention 'Geom.boundaryToPolygon' relies on.
rectCoords :: Int -> Int -> Int -> Int -> [Coordinate]
rectCoords minX minY maxX maxY =
  [coord minX minY, coord maxX minY, coord maxX maxY, coord minX maxY, coord minX minY]

boundaryOn :: Layer -> Int -> Int -> Int -> Int -> Boundary
boundaryOn lyr minX minY maxX maxY =
  Boundary { layer = lyr, coords = rectCoords minX minY maxX maxY }

cellWith :: String -> [Boundary] -> [TextDescription] -> [CellRef] -> Cell
cellWith nm bnds txts refs =
  Cell { name = nm, boundary = bnds, path = [], text = txts, cellRef = refs }

-- | An SREF with a translation only - no STRANS/ANGLE records.
srefAt :: String -> Int -> Int -> CellRef
srefAt nm dx dy =
  CellRef { name = nm, coord = coord dx dy, translation = Nothing, angle = Nothing }

-- | An SREF with a translation and a rotation, but no STRANS record.
srefRotated :: String -> Int -> Int -> Double -> CellRef
srefRotated nm dx dy ang =
  CellRef { name = nm, coord = coord dx dy, translation = Nothing, angle = Just ang }

textAt :: Layer -> Int -> Int -> String -> TextDescription
textAt lyr tx ty val = TextDescription
  { layer = lyr, coord = coord tx ty, value = val
  , translation = Nothing, angle = Nothing, presentation = Nothing, magnification = Nothing
  }

layerEntry :: String -> Int -> Int -> LayerEntry
layerEntry nm lyr dt = LayerEntry { name = nm, layer = lyr, datatype = dt }

-- | A LayerPolygon distinguished only by position - for connectedComponents
-- tests that need distinct, orderable node identities but no real
-- overlap/geometry.
node :: Int -> Int -> LayerPolygon
node minX minY = LayerPolygon "poly"
  (LabeledPolygon (boundaryToPolygon (boundaryOn (Layer 1 0) minX minY (minX + 1) (minY + 1))) "TOP")

relationshipTests :: TestTree
relationshipTests = testGroup "Relationship"
  [ testGroup "layerPolygons"
    [ testCase "own Boundary elements are grouped by Layer and labeled with the Cell's own name" $ do
        let lyrA = Layer { index = 1, kind = 0 }
            lyrB = Layer { index = 2, kind = 0 }
            bndA = boundaryOn lyrA 0 0 10 10
            bndB = boundaryOn lyrB 0 0 5 5
            top  = cellWith "TOP" [bndA, bndB] [] []
            grouped = layerPolygons [top] top
        Map.lookup lyrA grouped @?= Just [LabeledPolygon (boundaryToPolygon bndA) "TOP"]
        Map.lookup lyrB grouped @?= Just [LabeledPolygon (boundaryToPolygon bndB) "TOP"]

    , testCase "a Polygon pulled in via SREF is translated and labeled with the referenced Cell" $ do
        let lyr  = Layer { index = 1, kind = 0 }
            leaf = cellWith "LEAF" [boundaryOn lyr 0 0 10 10] [] []
            top  = cellWith "TOP" [] [] [srefAt "LEAF" 100 200]
            grouped  = layerPolygons [leaf, top] top
            expected = boundaryOn lyr 100 200 110 210
        Map.lookup lyr grouped @?= Just [LabeledPolygon (boundaryToPolygon expected) "LEAF"]

    , testCase "nested SREFs compose their translations, and the label stays at the innermost owning Cell" $ do
        let lyr  = Layer { index = 1, kind = 0 }
            leaf = cellWith "LEAF" [boundaryOn lyr 0 0 10 10] [] []
            mid  = cellWith "MID" [] [] [srefAt "LEAF" 5 5]
            top  = cellWith "TOP" [] [] [srefAt "MID" 100 100]
            grouped  = layerPolygons [leaf, mid, top] top
            expected = boundaryOn lyr 105 105 115 115
        Map.lookup lyr grouped @?= Just [LabeledPolygon (boundaryToPolygon expected) "LEAF"]

    , testCase "an SREF's rotation is applied about its own placement point before translating" $ do
        -- LEAF's 10x5 rectangle, rotated 90 degrees counter-clockwise about
        -- the origin, then placed at (50, 50): (0,0)->(50,50), (10,0)->(50,60),
        -- (10,5)->(45,60), (0,5)->(45,50).
        let lyr  = Layer { index = 1, kind = 0 }
            leaf = cellWith "LEAF" [boundaryOn lyr 0 0 10 5] [] []
            top  = cellWith "TOP" [] [] [srefRotated "LEAF" 50 50 90]
            grouped = layerPolygons [leaf, top] top
            expectedRing = [coord 50 50, coord 50 60, coord 45 60, coord 45 50, coord 50 50]
            expectedPolygon = boundaryToPolygon Boundary { layer = lyr, coords = expectedRing }
        Map.lookup lyr grouped @?= Just [LabeledPolygon expectedPolygon "LEAF"]
    ]

  , testGroup "pins"
    [ testCase "a text label anchored inside a same-numbered '.pin' layer Polygon becomes a Pin" $ do
        let pinLyr = Layer { index = 67, kind = 16 }
            lblLyr = Layer { index = 67, kind = 5 }
            lm = LayerMap
              { layers = [layerEntry "li1.pin" 67 16, layerEntry "li1.label" 67 5]
              , directConnections = []
              , crossConnections = []
              }
            pinB = boundaryOn pinLyr 0 0 10 10
            lbl  = textAt lblLyr 5 5 "OUT"
            top  = cellWith "TOP" [pinB] [lbl] []
        pins lm [top] top @?=
          [Pin { label = "OUT", polygon = LabeledPolygon (boundaryToPolygon pinB) "TOP", layer = pinLyr }]

    , testCase "a text label outside its would-be pin Polygon contributes no Pin" $ do
        let pinLyr = Layer { index = 67, kind = 16 }
            lblLyr = Layer { index = 67, kind = 5 }
            lm = LayerMap
              { layers = [layerEntry "li1.pin" 67 16, layerEntry "li1.label" 67 5]
              , directConnections = []
              , crossConnections = []
              }
            pinB = boundaryOn pinLyr 0 0 10 10
            lbl  = textAt lblLyr 50 50 "OUT"
            top  = cellWith "TOP" [pinB] [lbl] []
        pins lm [top] top @?= []

    , testCase "a layer whose name doesn't end in '.pin' never contributes a Pin, even if a label sits inside its shape" $ do
        let drawLyr = Layer { index = 67, kind = 20 }
            lblLyr  = Layer { index = 67, kind = 5 }
            lm = LayerMap
              { layers = [layerEntry "li1.drawing" 67 20, layerEntry "li1.label" 67 5]
              , directConnections = []
              , crossConnections = []
              }
            drawB = boundaryOn drawLyr 0 0 10 10
            lbl   = textAt lblLyr 5 5 "OUT"
            top   = cellWith "TOP" [drawB] [lbl] []
        pins lm [top] top @?= []

    , testCase "a pin and its label reached transitively through an SREF still match, both translated the same way" $ do
        let pinLyr = Layer { index = 67, kind = 16 }
            lblLyr = Layer { index = 67, kind = 5 }
            lm = LayerMap
              { layers = [layerEntry "li1.pin" 67 16, layerEntry "li1.label" 67 5]
              , directConnections = []
              , crossConnections = []
              }
            childPinB = boundaryOn pinLyr 0 0 10 10
            childLbl  = textAt lblLyr 5 5 "IN"
            child = cellWith "CHILD" [childPinB] [childLbl] []
            top   = cellWith "TOP" [] [] [srefAt "CHILD" 100 200]
            expected = boundaryOn pinLyr 100 200 110 210
        pins lm [child, top] top @?=
          [Pin { label = "IN", polygon = LabeledPolygon (boundaryToPolygon expected) "CHILD", layer = pinLyr }]
    ]

  , testGroup "pinsByInstance"
    [ testCase "a root Cell's own Pins group under its bare name; two SREF placements of the same Cell group separately" $ do
        let pinLyr = Layer { index = 67, kind = 16 }
            lblLyr = Layer { index = 67, kind = 5 }
            lm = LayerMap
              { layers = [layerEntry "li1.pin" 67 16, layerEntry "li1.label" 67 5]
              , directConnections = []
              , crossConnections = []
              }
            childPinB = boundaryOn pinLyr 0 0 10 10
            childLbl  = textAt lblLyr 5 5 "OUT"
            child     = cellWith "CHILD" [childPinB] [childLbl] []
            topPinB   = boundaryOn pinLyr 0 0 10 10
            topLbl    = textAt lblLyr 5 5 "TOP_PIN"
            top = cellWith "TOP" [topPinB] [topLbl] [srefAt "CHILD" 100 200, srefAt "CHILD" 500 600]
            result = pinsByInstance lm [child, top] top
            pinAt dx dy = Pin
              { label   = "OUT"
              , polygon = LabeledPolygon (boundaryToPolygon (boundaryOn pinLyr dx dy (dx + 10) (dy + 10))) "CHILD"
              , layer   = pinLyr
              }
        Map.keysSet result @?= Set.fromList ["TOP", "CHILD@(100,200)", "CHILD@(500,600)"]
        Map.lookup "TOP" result @?=
          Just [Pin { label = "TOP_PIN", polygon = LabeledPolygon (boundaryToPolygon topPinB) "TOP", layer = pinLyr }]
        Map.lookup "CHILD@(100,200)" result @?= Just [pinAt 100 200]
        Map.lookup "CHILD@(500,600)" result @?= Just [pinAt 500 600]
    ]

  , testGroup "connectivity"
    [ testCase "overlapping Polygons on a direct-connection layer are linked; an isolated Polygon on the same layer is not" $ do
        let polyLyr = Layer { index = 1, kind = 0 }
            polyA = boundaryOn polyLyr 0 0 10 10
            polyB = boundaryOn polyLyr 8 0 20 10
            polyC = boundaryOn polyLyr 100 100 110 110
            top   = cellWith "TOP" [polyA, polyB, polyC] [] []
            lm = LayerMap
              { layers = [layerEntry "poly.drawing" 1 0]
              , directConnections = [DirectConnection { layer = "poly" }]
              , crossConnections = []
              }
            lpA = LayerPolygon "poly" (LabeledPolygon (boundaryToPolygon polyA) "TOP")
            lpB = LayerPolygon "poly" (LabeledPolygon (boundaryToPolygon polyB) "TOP")
            result = connectivity lm [top] top
        Map.keysSet result @?= Set.fromList [lpA, lpB]
        Map.lookup lpA result @?= Just [lpB]
        Map.lookup lpB result @?= Just [lpA]

    , testCase "a cross connection links A and B through a via C, without ever recording A-C or B-C" $ do
        let polyLyr  = Layer { index = 1, kind = 0 }
            li1Lyr   = Layer { index = 2, kind = 0 }
            liconLyr = Layer { index = 3, kind = 0 }
            polyA = boundaryOn polyLyr 0 0 10 10
            li1B  = boundaryOn li1Lyr 5 5 15 15
            viaC  = boundaryOn liconLyr 7 7 9 9
            top   = cellWith "TOP" [polyA, li1B, viaC] [] []
            lm = LayerMap
              { layers =
                  [ layerEntry "poly.drawing" 1 0
                  , layerEntry "li1.drawing" 2 0
                  , layerEntry "licon1.drawing" 3 0
                  ]
              , directConnections = []
              , crossConnections = [CrossConnection { layers = ["poly", "li1"], via = "licon1" }]
              }
            lpA = LayerPolygon "poly" (LabeledPolygon (boundaryToPolygon polyA) "TOP")
            lpB = LayerPolygon "li1" (LabeledPolygon (boundaryToPolygon li1B) "TOP")
            result = connectivity lm [top] top
        Map.keysSet result @?= Set.fromList [lpA, lpB]
        Map.lookup lpA result @?= Just [lpB]
        Map.lookup lpB result @?= Just [lpA]

    , testCase "a named layer's polygons are gathered across every datatype under its dot-prefix" $ do
        let drawLyr = Layer { index = 1, kind = 0 }
            gateLyr = Layer { index = 1, kind = 9 }
            polyDrawing = boundaryOn drawLyr 0 0 10 10
            polyGate    = boundaryOn gateLyr 8 0 20 10
            top = cellWith "TOP" [polyDrawing, polyGate] [] []
            lm = LayerMap
              { layers = [layerEntry "poly.drawing" 1 0, layerEntry "poly.gate" 1 9]
              , directConnections = [DirectConnection { layer = "poly" }]
              , crossConnections = []
              }
            lpDrawing = LayerPolygon "poly" (LabeledPolygon (boundaryToPolygon polyDrawing) "TOP")
            lpGate    = LayerPolygon "poly" (LabeledPolygon (boundaryToPolygon polyGate) "TOP")
            result = connectivity lm [top] top
        Map.keysSet result @?= Set.fromList [lpDrawing, lpGate]

    , testCase "a cross_connections entry naming other than two layers is an error" $ do
        let lm = LayerMap
              { layers = []
              , directConnections = []
              , crossConnections = [CrossConnection { layers = ["poly"], via = "licon1" }]
              }
            top = cellWith "TOP" [] [] []
        result <- try (evaluate (connectivity lm [top] top))
          :: IO (Either SomeException (Map.Map LayerPolygon [LayerPolygon]))
        case result of
          Left _  -> return ()
          Right v -> assertFailure ("expected an error, got " ++ show v)
    ]

  , testGroup "connectedComponents"
    [ testCase "nodes linked by a path of edges share a component id; a separate edge forms a different one" $ do
        let a = node 0 0
            b = node 10 10
            c = node 20 20
            d = node 30 30
            e = node 40 40
            adj = Map.fromList
              [ (a, [b])
              , (b, [a, c])
              , (c, [b])
              , (d, [e])
              , (e, [d])
              ]
            result = connectedComponents adj
        Map.lookup a result @?= Map.lookup b result
        Map.lookup b result @?= Map.lookup c result
        Map.lookup d result @?= Map.lookup e result
        assertBool "disjoint edges get different component ids"
          (Map.lookup a result /= Map.lookup d result)

    , testCase "a node that only appears in another node's edge list is still assigned a component" $ do
        let a = node 0 0
            b = node 10 10
            adj = Map.fromList [ (a, [b]) ]
            result = connectedComponents adj
        Map.member b result @?= True
        Map.lookup a result @?= Map.lookup b result

    , testCase "computed from connectivity's own adjacency list: overlapping pairs share a component, disjoint pairs don't" $ do
        let polyLyr = Layer { index = 1, kind = 0 }
            polyA = boundaryOn polyLyr 0 0 10 10
            polyB = boundaryOn polyLyr 8 0 20 10
            polyC = boundaryOn polyLyr 100 100 110 110
            polyD = boundaryOn polyLyr 108 100 120 110
            top   = cellWith "TOP" [polyA, polyB, polyC, polyD] [] []
            lm = LayerMap
              { layers = [layerEntry "poly.drawing" 1 0]
              , directConnections = [DirectConnection { layer = "poly" }]
              , crossConnections = []
              }
            adj   = connectivity lm [top] top
            comps = connectedComponents adj
            lpA = LayerPolygon "poly" (LabeledPolygon (boundaryToPolygon polyA) "TOP")
            lpB = LayerPolygon "poly" (LabeledPolygon (boundaryToPolygon polyB) "TOP")
            lpC = LayerPolygon "poly" (LabeledPolygon (boundaryToPolygon polyC) "TOP")
            lpD = LayerPolygon "poly" (LabeledPolygon (boundaryToPolygon polyD) "TOP")
        Map.lookup lpA comps @?= Map.lookup lpB comps
        Map.lookup lpC comps @?= Map.lookup lpD comps
        assertBool "disjoint overlapping pairs get different component ids"
          (Map.lookup lpA comps /= Map.lookup lpC comps)
    ]
  ]

-- === Relationship.netlist ===================================================

-- | A Component declaring exactly the given pin names, on an unused
-- dummy layer - 'netlist' never reads a declared Pin's own 'layer' field,
-- only its 'name', to decide which of a recognized component's
-- geometrically-found Pins count as netlist nodes at all (see
-- 'Relationship.netlist''s own "declared" documentation).
componentDeclaring :: [String] -> Component.Component
componentDeclaring names = Component.Component
  { Component.componentType = ""
  , Component.pins = [ Component.Pin { Component.name = nm, Component.layer = "" } | nm <- names ]
  }

netlistTests :: TestTree
netlistTests = testGroup "Relationship.netlist"
  [ testCase "fans out through a chain of recognized components, pairing every pin sharing a net" $ do
      -- TOP.OUT overlaps GATE1 (INV@(0,0))'s A pin directly; GATE1's own
      -- Y pin is bridged to GATE2 (INV@(50,0))'s A pin by a plain wire
      -- shape (not itself a pin, so it never appears in the netlist);
      -- GATE2's Y pin touches nothing else, so the chain ends there.
      -- GATE1's Y and GATE2's A are placed at different absolute
      -- coordinates deliberately - were they to coincide exactly, both
      -- being untransformed copies of the very same INV Cell, their
      -- LabeledPolygons (same ring, same parent Cell name "INV") would
      -- collide into a single graph node, which would defeat the point
      -- of this test.
      let pinLyr  = Layer { index = 67, kind = 16 }
          drawLyr = Layer { index = 67, kind = 20 }
          lblLyr  = Layer { index = 67, kind = 5 }
          lm = LayerMap
            { layers =
                [ layerEntry "li1.pin" 67 16
                , layerEntry "li1.drawing" 67 20
                , layerEntry "li1.label" 67 5
                ]
            , directConnections = [DirectConnection { layer = "li1" }]
            , crossConnections = []
            }
          compList = Map.fromList
            [ ("TOP", componentDeclaring ["OUT"])
            , ("INV", componentDeclaring ["A", "Y"])
            ]
          invCell = cellWith "INV"
            [boundaryOn pinLyr 0 0 10 10, boundaryOn pinLyr 20 0 30 10]
            [textAt lblLyr 5 5 "A", textAt lblLyr 25 5 "Y"]
            []
          top = cellWith "TOP"
            [boundaryOn pinLyr 0 0 10 10, boundaryOn drawLyr 25 0 55 10]
            [textAt lblLyr 5 5 "OUT"]
            [srefAt "INV" 0 0, srefAt "INV" 50 0]
          result = netlist lm compList [invCell, top] top "OUT"
          expected = Set.fromList
            [ (("INV@(0,0)", "A"), ("TOP", "OUT"))
            , (("INV@(0,0)", "Y"), ("INV@(50,0)", "A"))
            ]
      result @?= expected

  , testCase "a pin absent from the component's own declared pin list is never fanned out to" $ do
      -- Same layout as the chain test above, but INV's components-file
      -- entry only declares "A" - so even though GATE1's Y pin is found
      -- geometrically (and would, if declared, bridge on to GATE2), the
      -- search never fans out to it and the chain stops at GATE1.A.
      let pinLyr  = Layer { index = 67, kind = 16 }
          drawLyr = Layer { index = 67, kind = 20 }
          lblLyr  = Layer { index = 67, kind = 5 }
          lm = LayerMap
            { layers =
                [ layerEntry "li1.pin" 67 16
                , layerEntry "li1.drawing" 67 20
                , layerEntry "li1.label" 67 5
                ]
            , directConnections = [DirectConnection { layer = "li1" }]
            , crossConnections = []
            }
          compList = Map.fromList
            [ ("TOP", componentDeclaring ["OUT"])
            , ("INV", componentDeclaring ["A"])
            ]
          invCell = cellWith "INV"
            [boundaryOn pinLyr 0 0 10 10, boundaryOn pinLyr 20 0 30 10]
            [textAt lblLyr 5 5 "A", textAt lblLyr 25 5 "Y"]
            []
          top = cellWith "TOP"
            [boundaryOn pinLyr 0 0 10 10, boundaryOn drawLyr 25 0 55 10]
            [textAt lblLyr 5 5 "OUT"]
            [srefAt "INV" 0 0, srefAt "INV" 50 0]
          result = netlist lm compList [invCell, top] top "OUT"
          expected = Set.fromList [ (("INV@(0,0)", "A"), ("TOP", "OUT")) ]
      result @?= expected

  , testCase "a pin on an unrecognized SREF instance never appears in the netlist at all" $ do
      -- Same layout as above, but the components file doesn't name "INV",
      -- so GATE1's A pin - though geometrically found, and touching
      -- TOP.OUT's own net - never counts as a declared pin at all: it's
      -- not fanned out to, and it's not even reported as a connection
      -- endpoint.
      let pinLyr = Layer { index = 67, kind = 16 }
          lblLyr = Layer { index = 67, kind = 5 }
          lm = LayerMap
            { layers = [layerEntry "li1.pin" 67 16, layerEntry "li1.label" 67 5]
            , directConnections = [DirectConnection { layer = "li1" }]
            , crossConnections = []
            }
          compList = Map.fromList [("TOP", componentDeclaring ["OUT"])]
          invCell = cellWith "INV"
            [boundaryOn pinLyr 0 0 10 10, boundaryOn pinLyr 20 0 30 10]
            [textAt lblLyr 5 5 "A", textAt lblLyr 25 5 "Y"]
            []
          top = cellWith "TOP"
            [boundaryOn pinLyr 0 0 10 10]
            [textAt lblLyr 5 5 "OUT"]
            [srefAt "INV" 0 0, srefAt "INV" 20 0]
          result = netlist lm compList [invCell, top] top "OUT"
      result @?= Set.empty

  , testCase "a pin name absent from the top cell's own pins is an error" $ do
      let top = cellWith "TOP" [] [] []
          lm = LayerMap { layers = [], directConnections = [], crossConnections = [] }
      result <- try (evaluate (netlist lm Map.empty [top] top "NOPE"))
        :: IO (Either SomeException (Set.Set ((String, String), (String, String))))
      case result of
        Left _  -> return ()
        Right v -> assertFailure ("expected an error, got " ++ show v)
  ]

-- === Component ==============================================================

componentTests :: TestTree
componentTests = testGroup "Component"
  [ testCase "readComponentList parses a component-name-keyed YAML file" $ do
      result <- readComponentList "test/fixtures/components.yaml"
      let expectedInv1 = Component.Component
            { Component.componentType = "inverter"
            , Component.pins =
                [ Component.Pin { Component.name = "A", Component.layer = "li1" }
                , Component.Pin { Component.name = "Y", Component.layer = "li1" }
                , Component.Pin { Component.name = "VPWR", Component.layer = "met1" }
                , Component.Pin { Component.name = "VGND", Component.layer = "met1" }
                ]
            }
          expectedNand2 = Component.Component
            { Component.componentType = "nand2"
            , Component.pins =
                [ Component.Pin { Component.name = "A", Component.layer = "li1" }
                , Component.Pin { Component.name = "B", Component.layer = "li1" }
                , Component.Pin { Component.name = "Y", Component.layer = "li1" }
                ]
            }
      Map.keysSet result @?= Set.fromList ["inv1", "nand2_1"]
      Map.lookup "inv1" result @?= Just expectedInv1
      Map.lookup "nand2_1" result @?= Just expectedNand2
  ]
