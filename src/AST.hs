module AST
  ( AST(..)
  , astKind
  , buildForest
  , isOpener
  , isCloser
  , findChild
  , findChildren
  ) where

import           Parse (GdsRecord, GdsRecordT (..), gdsRecordKind)

-- | A GDS record together with the records nested beneath it - e.g. the
-- elements between a BOUNDARY and its ENDEL, or the structures between a
-- BGNLIB and its ENDLIB. This only captures the nesting shape of a record
-- stream; it doesn't interpret what the nesting means.
data AST = AST
  { astRecord   :: GdsRecordT
  , astChildren :: [AST]
  } deriving (Show)

astKind :: AST -> GdsRecord
astKind = gdsRecordKind . astRecord

-- | The first child of the given kind, if present.
findChild :: GdsRecord -> [AST] -> Maybe AST
findChild kind = find ((== kind) . astKind)

-- | All children of the given kind, in order.
findChildren :: GdsRecord -> [AST] -> [AST]
findChildren kind = filter ((== kind) . astKind)

-- | Groups a flat stream of records into a forest by matching each
-- "begin" record (BGNLIB, BGNSTR, or an element record) with the "end"
-- record that closes it (ENDLIB, ENDSTR, ENDEL). A stray end record with
-- no matching begin, or a begin record left open at end of input, is a
-- malformed record stream and is a hard error.
buildForest :: [GdsRecordT] -> [AST]
buildForest = finalize . foldl' step ([], [])
  where
    step (forest, stack) r
      | isOpener r = (forest, (r, []) : stack)
      | isCloser r = case stack of
          (rec0, kids) : rest -> closeInto rest forest rec0 (AST r [] : kids)
          []                  -> astError ("stray closing record with no matching opener: " ++ show r)
      | otherwise = case stack of
          (rec0, kids) : rest -> (forest, (rec0, AST r [] : kids) : rest)
          []                  -> (AST r [] : forest, [])

    closeInto rest forest rec0 kids = case rest of
      (rec1, kids1) : rest' -> (forest, (rec1, AST rec0 (reverse kids) : kids1) : rest')
      []                    -> (AST rec0 (reverse kids) : forest, [])

    finalize (forest, [])              = reverse forest
    finalize (_, (rec0, _) : _)        = astError ("unclosed opening record: " ++ show rec0)

-- | error requires Text (relude's Prelude); this is the single conversion
-- point for buildForest's malformed-stream errors.
astError :: String -> a
astError = error . toText

isOpener :: GdsRecordT -> Bool
isOpener r = case r of
  GdsBgnLibT{} -> True
  GdsBgnStrT{} -> True
  GdsBoundaryT -> True
  GdsPathT     -> True
  GdsSrefT     -> True
  GdsArefT     -> True
  GdsTextT     -> True
  _            -> False

isCloser :: GdsRecordT -> Bool
isCloser r = case r of
  GdsEndLibT -> True
  GdsEndStrT -> True
  GdsEndElT  -> True
  _          -> False
