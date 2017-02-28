{-|
Module      : Geometry.Shapefile.Types
Description : Type definitions used by the library. Currently (24 Nov 2015)
              MultiPatch record types are not implemented. Only Point and
              Polygon are (somewhat) tested. YMMV
              https://www.esri.com/library/whitepapers/pdfs/shapefile.pdf
Author      : Sam van Herwaarden <samvherwaarden@gmail.com>
-}

module Geometry.Shapefile.Types where

import qualified Data.ByteString as BS

import Geometry.Shapefile.Internal

-- * SHP Types

data ShpData =
  ShpData {
    shpHeader     :: ShpHeader,
    dbfFieldDescs :: Maybe [DbfFieldDesc],
    shpRecs       :: [ShpRec]
  } deriving (Eq, Show)

data ShpHeader =
  ShpHeader {
    shpFileLength :: Int,
    shpVersion    :: Int,
    shpType       :: ShpType,
    shpBB         :: ShpBBox
  } deriving (Eq, Show)

data ShpType = ShpNull
             | ShpPoint
             | ShpPolyLine
             | ShpPolygon
             | ShpMultiPoint
             | ShpPointZ
             | ShpPolyLineZ
             | ShpPolygonZ
             | ShpMultiPointZ
             | ShpPointM
             | ShpPolyLineM
             | ShpPolygonM
             | ShpMultiPointM
             | ShpMultiPatch
               deriving (Eq, Show)

-- | Types with Z values
zTypes :: [ShpType]
zTypes = [ShpPointZ, ShpPolyLineZ, ShpPolygonZ, ShpMultiPointZ]

-- | Types with M values
mTypes :: [ShpType]
mTypes = [ShpPointM, ShpPolyLineM, ShpPolygonM, ShpMultiPointM]

-- | Numeric codes used for record types
shpTypeFromId :: Int -> ShpType
shpTypeFromId i =
  case i of 0  -> ShpNull
            1  -> ShpPoint
            3  -> ShpPolyLine
            5  -> ShpPolygon
            8  -> ShpMultiPoint
            11 -> ShpPointZ
            13 -> ShpPolyLineZ
            15 -> ShpPolygonZ
            18 -> ShpMultiPointZ
            21 -> ShpPointM
            23 -> ShpPolyLineM
            25 -> ShpPolygonM
            28 -> ShpMultiPointM
            31 -> ShpMultiPatch
            _  -> error "shpTypeFromId: Unknown Shape Type"

data ShpBBox =
  ShpBBox {
    shpXMin :: Double,
    shpXMax :: Double,
    shpYMin :: Double,
    shpYMax :: Double,
    shpZMin :: Maybe Double,
    shpZMax :: Maybe Double,
    shpMMin :: Maybe Double,
    shpMMax :: Maybe Double
  } deriving (Eq, Show)


data ShpRec =
  ShpRec {
    shpRecNum      :: Int,
    shpRecLen      :: Int,
    shpRecContents :: Maybe RecContents, -- Can be null type
    shpRecLabel    :: Maybe [DbfRecord], -- Should be usable without dbf
    shpRecType     :: ShpType
  } deriving (Eq, Show)

data RecContents =
    RecNull
  | RecPoint Point
  | RecPointM {
      pmPoint             :: Point,
      pmM                 :: Double }
  | RecPointZ {
      pzPoint             :: Point,
      pzZ                 :: Double,
      pzM                 :: Double }
  | RecMultiPoint {
      recMPBBox           :: RecBBox,
      recMPNumPoints      :: Int,
      recMPPoints         :: [Point] }
  | RecMultiPointM {
      recMPMBBox          :: RecBBox,
      recMPMNumPoints     :: Int,
      recMPMPoints        :: [Point],
      recMPMMRange        :: (Double, Double),
      recMPMMs            :: [Double] }
  | RecMultiPointZ {
      recMPZBBox          :: RecBBox,
      recMPZNumPoints     :: Int,
      recMPZPoints        :: [Point],
      recMPZZRange        :: (Double, Double),
      recMPZZs            :: [Double],
      recMPZMRange        :: (Double, Double),
      recMPZMs            :: [Double] }
  | RecPolyLine {
      recPolLBBox         :: RecBBox,
      recPolLNumParts     :: Int,
      recPolLNumPoints    :: Int,
      recPolLPartLengths  :: [Int],
      recPolLPoints       :: [[Point]] }
  | RecPolyLineM {
      recPolLMBBox        :: RecBBox,
      recPolLMNumParts    :: Int,
      recPolLMNumPoints   :: Int,
      recPolLMPartLengths :: [Int],
      recPolLMPoints      :: [[Point]],
      recPolLMMRange      :: (Double, Double),
      recPolLMMs          :: [Double] }
  | RecPolyLineZ {
      recPolLZBBox        :: RecBBox,
      recPolLZNumParts    :: Int,
      recPolLZNumPoints   :: Int,
      recPolLZPartLengths :: [Int],
      recPolLZPoints      :: [[Point]],
      recPolLZZRange      :: (Double, Double),
      recPolLZZs          :: [Double],
      recPolLZMRange      :: (Double, Double),
      recPolLZMs          :: [Double] }
  | RecPolygon {
      recPolBBox          :: RecBBox,
      recPolNumParts      :: Int,
      recPolNumPoints     :: Int,
      recPolPartLengths   :: [Int],
      recPolPoints        :: [[Point]] }
  | RecPolygonM {
      recPolMBBox         :: RecBBox,
      recPolMNumParts     :: Int,
      recPolMNumPoints    :: Int,
      recPolMPartLengths  :: [Int],
      recPolMPoints       :: [[Point]],
      recPolMMRange       :: (Double, Double),
      recPolMMs           :: [Double] }
  | RecPolygonZ {
      recPolZBBox         :: RecBBox,
      recPolZNumParts     :: Int,
      recPolZNumPoints    :: Int,
      recPolZPartLengths  :: [Int],
      recPolZPoints       :: [[Point]],
      recPolZZRange       :: (Double, Double),
      recPolZZs           :: [Double],
      recPolZMRange       :: (Double, Double),
      recPolZMs           :: [Double] }
  -- RecMultiPatch
    deriving (Eq, Show)

data RecBBox =
  RecBBox {
    recXMin :: Double,
    recXMax :: Double,
    recYMin :: Double,
    recYMax :: Double
  } deriving (Eq, Show)

-- * DBF Types

data DbfData =
  DbfData {
    dbfNumRecs :: Int,
    dbfFields :: [DbfFieldDesc],
    dbfRecords :: [[DbfRecord]]
  } deriving (Eq, Show)

data DbfFieldDesc =
  DbfFieldDesc {
    fieldName :: String,
    fieldType :: Char,
    fieldLen :: Int
  } deriving (Eq, Show)

-- | Not all record types are implemented. Use your favorite method to
--   convert any bytestring (and feel free to add functionality).
data DbfRecord = DbfString String
               | DbfNum String
               | DbfBS BS.ByteString
                 deriving (Eq, Show)

