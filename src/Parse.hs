{-# LANGUAGE OverloadedStrings #-}

module Parse where

import qualified Data.Attoparsec.ByteString as DAP
import           Data.Bits                  (Bits (shiftR, (.&.)), testBit)
import           Data.Serialize.Get         (getInt16be, getInt32be,
                                             getWord64be, runGet)
import           Data.Time                  (LocalTime (..), TimeOfDay (..),
                                             fromGregorian)

-- | Types that can be decoded from the single-byte codes GDSII uses for
-- record types and data types. The mapping is failable since not every
-- byte value corresponds to a known/supported code; the error names the
-- type being decoded.
class FromGdsWord8 a where
  fromGdsWord8 :: Word8 -> Either String a

data GdsDataType =
  GdsNoData
  | GdsBitArray
  | GdsInt16
  | GdsInt32
  | GdsReal64
  | GdsAscii
  deriving (Show, Eq)

instance FromGdsWord8 GdsDataType where
  fromGdsWord8 0 = Right GdsNoData
  fromGdsWord8 1 = Right GdsBitArray
  fromGdsWord8 2 = Right GdsInt16
  fromGdsWord8 3 = Right GdsInt32
  fromGdsWord8 5 = Right GdsReal64
  fromGdsWord8 6 = Right GdsAscii
  fromGdsWord8 x = Left $ "invalid GdsDataType code: " ++ show x

data GdsRecord =
  GdsHeader
  | GdsBgnLib
  | GdsLibName
  | GdsUnits
  | GdsEndLib
  | GdsBgnStr
  | GdsStrName
  | GdsEndStr
  | GdsBoundary
  | GdsPath
  | GdsSref
  | GdsAref
  | GdsText
  | GdsLayer
  | GdsDataType
  | GdsWidth
  | GdsXy
  | GdsEndEl
  | GdsSname
  | GdsTextType
  | GdsPresentation
  | GdsString
  | GdsStrans
  | GdsMag
  | GdsAngle
  | GdsPathType
  | GdsBgnExtn
  | GdsEndExtn
  deriving (Show)

instance FromGdsWord8 GdsRecord where
  fromGdsWord8 0  = Right GdsHeader
  fromGdsWord8 1  = Right GdsBgnLib
  fromGdsWord8 2  = Right GdsLibName
  fromGdsWord8 3  = Right GdsUnits
  fromGdsWord8 4  = Right GdsEndLib
  fromGdsWord8 5  = Right GdsBgnStr
  fromGdsWord8 6  = Right GdsStrName
  fromGdsWord8 7  = Right GdsEndStr
  fromGdsWord8 8  = Right GdsBoundary
  fromGdsWord8 9  = Right GdsPath
  fromGdsWord8 10 = Right GdsSref
  fromGdsWord8 11 = Right GdsAref
  fromGdsWord8 12 = Right GdsText
  fromGdsWord8 13 = Right GdsLayer
  fromGdsWord8 14 = Right GdsDataType
  fromGdsWord8 15 = Right GdsWidth
  fromGdsWord8 16 = Right GdsXy
  fromGdsWord8 17 = Right GdsEndEl
  fromGdsWord8 18 = Right GdsSname
  fromGdsWord8 22 = Right GdsTextType
  fromGdsWord8 23 = Right GdsPresentation
  fromGdsWord8 25 = Right GdsString
  fromGdsWord8 26 = Right GdsStrans
  fromGdsWord8 27 = Right GdsMag
  fromGdsWord8 28 = Right GdsAngle
  fromGdsWord8 33 = Right GdsPathType
  fromGdsWord8 48 = Right GdsBgnExtn
  fromGdsWord8 49 = Right GdsEndExtn
  fromGdsWord8 x  = Left $ "invalid GdsRecord code: " ++ show x

data GdsRecordT =
  GdsHeaderT Int
  | GdsLibNameT String
  | GdsBgnLibT GdsDateTime
  | GdsBgnStrT GdsDateTime
  | GdsUnitsT Double Double
  | GdsEndLibT
  | GdsStrNameT String
  | GdsEndStrT
  | GdsBoundaryT
  | GdsPathT
  | GdsSrefT
  | GdsArefT
  | GdsTextT
  | GdsLayerT Int
  | GdsDataTypeT Int
  | GdsWidthT Int
  | GdsXyT [(Int32, Int32)]
  | GdsEndElT
  | GdsSnameT String
  | GdsTextTypeT Int
  | GdsPresentationT GdsPresentationFlags
  | GdsStringT String
  | GdsStransT GdsStransFlags
  | GdsMagT Double
  | GdsAngleT Double
  | GdsPathTypeT Int
  | GdsBgnExtnT Int
  | GdsEndExtnT Int
  deriving (Show)

data GdsDateTime = GdsDateTime
  { gdsModTime :: LocalTime
  , gdsAccTime :: LocalTime
  } deriving (Show, Eq)

data GdsStransFlags = GdsStransFlags
  { gdsMirrorX  :: Bool
  , gdsAbsMag   :: Bool
  , gdsAbsAngle :: Bool
  } deriving (Show, Eq)

data GdsPresentationFlags = GdsPresentationFlags
  { gdsFont      :: Int
  , gdsVertJust  :: Int
  , gdsHorizJust :: Int
  } deriving (Show, Eq)

-- Conversion helpers

failCerealGet :: Either String a -> a
failCerealGet res = case res of
  Left msg -> error $ toText msg
  Right x  -> x

failToEnum :: FromGdsWord8 a => Word8 -> a
failToEnum x = case fromGdsWord8 x of
  Right p  -> p
  Left msg -> error $ toText msg

gdsRealToDouble :: Word64 -> Double
gdsRealToDouble 0 = 0.0
gdsRealToDouble w = sign * fraction * (16 ** exponent)
  where
    sign = if testBit w 63 then -1.0 else 1.0
    exponent = fromIntegral ((w `shiftR` 56) .&. 0x7F) - 64
    mantissa = fromIntegral (w .&. 0x00FFFFFFFFFFFFFF)
    fraction = mantissa / (2 ** 56)

-- STRANS bits are numbered from the MSB (spec bit 0); the field is parsed as
-- a plain 16-bit value, so spec bit N is testBit value (15 - N).
decodeGdsStransFlags :: Int -> GdsStransFlags
decodeGdsStransFlags bits = GdsStransFlags
  { gdsMirrorX  = testBit bits 15
  , gdsAbsMag   = testBit bits 2
  , gdsAbsAngle = testBit bits 1
  }

-- PRESENTATION bits are numbered from the MSB (spec bit 0), same convention
-- as STRANS. Spec bits 10-11 hold the font number, 12-13 the vertical
-- justification (0 top, 1 middle, 2 bottom), 14-15 the horizontal
-- justification (0 left, 1 center, 2 right) -- i.e. value bits 5-4, 3-2, 1-0.
decodeGdsPresentationFlags :: Int -> GdsPresentationFlags
decodeGdsPresentationFlags bits = GdsPresentationFlags
  { gdsFont      = (bits `shiftR` 4) .&. 0x3
  , gdsVertJust  = (bits `shiftR` 2) .&. 0x3
  , gdsHorizJust = bits .&. 0x3
  }

-- BgnLib/BgnStr timestamp field is years since 1900 (e.g. 126), but some
-- writers emit the full year (e.g. 2026); normalize both to a full year.
normalizeGdsYear :: Int -> Int
normalizeGdsYear y
  | y < 1900  = y + 1900
  | otherwise = y

mkGdsLocalTime :: Int -> Int -> Int -> Int -> Int -> Int -> LocalTime
mkGdsLocalTime y mo d h mi s =
  LocalTime
    (fromGregorian (fromIntegral (normalizeGdsYear y)) mo d)
    (TimeOfDay h mi (fromIntegral s))

parseGdsDateTime :: [Int] -> GdsDateTime
parseGdsDateTime [my, mm, md, mh, mmin, ms, ay, am, ad, ah, amin, as] =
  GdsDateTime
    (mkGdsLocalTime my mm md mh mmin ms)
    (mkGdsLocalTime ay am ad ah amin as)
parseGdsDateTime xs =
  error $ toText ("expected 12 Ints for GdsDateTime, got " ++ show (length xs))

-- Parser building blocks

parseInt16 :: DAP.Parser Int16
parseInt16 = DAP.take 2 <&> failCerealGet . runGet getInt16be

parseInt16AsInt :: DAP.Parser Int
parseInt16AsInt = parseInt16 <&> fromIntegral

parseInt32 :: DAP.Parser Int32
parseInt32 = DAP.take 4 <&> failCerealGet . runGet getInt32be

parseInt32AsInt :: DAP.Parser Int
parseInt32AsInt = parseInt32 <&> fromIntegral

parseWord64 :: DAP.Parser Word64
parseWord64 = DAP.take 8 <&> failCerealGet . runGet getWord64be

parseCharEnum :: FromGdsWord8 a => DAP.Parser a
parseCharEnum = DAP.anyWord8 <&> failToEnum

parseGdsReal :: DAP.Parser Double
parseGdsReal = parseWord64 <&> gdsRealToDouble

-- GDSII pads odd-length Ascii fields with a single trailing NUL; the spec
-- guarantees NUL only ever appears as that trailing pad, so this also
-- correctly strips it.
parseAsciiString :: Int -> DAP.Parser String
parseAsciiString len = DAP.take len <&> takeWhile (/= '\NUL') . decodeUtf8

-- Parse various record fields

parseLength :: DAP.Parser Int
parseLength = parseInt16AsInt

parseRecordType :: DAP.Parser GdsRecord
parseRecordType = parseCharEnum

parseDataType :: DAP.Parser GdsDataType
parseDataType = parseCharEnum

-- Checks the payload's actual length/data-type against what's expected for
-- the record being parsed, then runs the parser or fails with a message
-- built from the mismatch.
parseChecked :: Int -> GdsDataType -> Int -> GdsDataType -> DAP.Parser a -> DAP.Parser a
parseChecked expectedLen expectedType actualLen actualType p
  | actualLen == expectedLen && actualType == expectedType = p
  | otherwise = fail $
      "expected " ++ show expectedType ++ " payload of len " ++ show expectedLen
      ++ ", got len " ++ show actualLen ++ " type " ++ show actualType

-- Parse record
parseGdsRecord :: DAP.Parser GdsRecordT
parseGdsRecord = do
  recLen <- parseLength
  recType <- parseRecordType
  dataType <- parseDataType
  let
    payLen = recLen - 4
    checked = parseChecked payLen dataType payLen dataType
  case recType of
    GdsHeader ->
      parseChecked 2 GdsInt16 payLen dataType $
        parseInt16AsInt <&> GdsHeaderT
    GdsBgnLib ->
      parseChecked 24 GdsInt16 payLen dataType $
        DAP.count 12 parseInt16AsInt <&> GdsBgnLibT . parseGdsDateTime
    GdsLibName ->
      checked $ parseAsciiString payLen <&> GdsLibNameT
    GdsUnits ->
      parseChecked 16 GdsReal64 payLen dataType $
        GdsUnitsT <$> parseGdsReal <*> parseGdsReal
    GdsEndLib ->
      parseChecked 0 GdsNoData payLen dataType $ pure GdsEndLibT
    GdsBgnStr ->
      parseChecked 24 GdsInt16 payLen dataType $
        DAP.count 12 parseInt16AsInt <&> GdsBgnStrT . parseGdsDateTime
    GdsStrName ->
      checked $ parseAsciiString payLen <&> GdsStrNameT
    GdsEndStr ->
      parseChecked 0 GdsNoData payLen dataType $ pure GdsEndStrT
    GdsBoundary ->
      parseChecked 0 GdsNoData payLen dataType $ pure GdsBoundaryT
    GdsPath ->
      parseChecked 0 GdsNoData payLen dataType $ pure GdsPathT
    GdsSref ->
      parseChecked 0 GdsNoData payLen dataType $ pure GdsSrefT
    GdsAref ->
      parseChecked 0 GdsNoData payLen dataType $ pure GdsArefT
    GdsText ->
      parseChecked 0 GdsNoData payLen dataType $ pure GdsTextT
    GdsLayer ->
      parseChecked 2 GdsInt16 payLen dataType $
        parseInt16AsInt <&> GdsLayerT
    GdsDataType ->
      parseChecked 2 GdsInt16 payLen dataType $
        parseInt16AsInt <&> GdsDataTypeT
    GdsWidth ->
      parseChecked 4 GdsInt32 payLen dataType $
        parseInt32AsInt <&> GdsWidthT
    GdsXy ->
      if dataType /= GdsInt32 || payLen `mod` 8 /= 0
      then fail $
        "expected Int32 payload with length a multiple of 8, got len "
        ++ show payLen ++ " type " ++ show dataType
      else DAP.count (payLen `div` 8) ((,) <$> parseInt32 <*> parseInt32) <&> GdsXyT
    GdsEndEl ->
      parseChecked 0 GdsNoData payLen dataType $ pure GdsEndElT
    GdsSname ->
      checked $ parseAsciiString payLen <&> GdsSnameT
    GdsTextType ->
      parseChecked 2 GdsInt16 payLen dataType $
        parseInt16AsInt <&> GdsTextTypeT
    GdsPresentation ->
      parseChecked 2 GdsBitArray payLen dataType $
        parseInt16AsInt <&> GdsPresentationT . decodeGdsPresentationFlags
    GdsString ->
      checked $ parseAsciiString payLen <&> GdsStringT
    GdsStrans ->
      parseChecked 2 GdsBitArray payLen dataType $
        parseInt16AsInt <&> GdsStransT . decodeGdsStransFlags
    GdsMag ->
      parseChecked 8 GdsReal64 payLen dataType $
        parseGdsReal <&> GdsMagT
    GdsAngle ->
      parseChecked 8 GdsReal64 payLen dataType $
        parseGdsReal <&> GdsAngleT
    GdsPathType ->
      parseChecked 2 GdsInt16 payLen dataType $
        parseInt16AsInt <&> GdsPathTypeT
    GdsBgnExtn ->
      parseChecked 4 GdsInt32 payLen dataType $
        parseInt32AsInt <&> GdsBgnExtnT
    GdsEndExtn ->
      parseChecked 4 GdsInt32 payLen dataType $
        parseInt32AsInt <&> GdsEndExtnT
