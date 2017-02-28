{-|
Module      : Geometry.Shapefile.ReadShp
Description : Code for reading the binary ESRI Shapefile format.
              https://www.esri.com/library/whitepapers/pdfs/shapefile.pdf
Author      : Sam van Herwaarden <samvherwaarden@gmail.com>
-}

module Geometry.Shapefile.ReadShp ( readShpFile,
                                    readShpData,
                                  ) where

import Control.Monad       ( replicateM, when )
import Control.Monad.Loops ( whileM )
import Data.Binary.Get
import Data.Binary.IEEE754 ( getFloat64le )

import qualified Data.ByteString.Lazy as BL

import Geometry.Shapefile.Internal
import Geometry.Shapefile.Types

-- | Read shapefile at `fp` to resident data structure `ShpData`
readShpFile :: String -> IO ShpData
readShpFile fp = readShpData <$> BL.readFile fp

-- | Parse a shapefile ByteString
readShpData :: BL.ByteString -> ShpData
readShpData = runGet $ do
  shpH  <- getShpHeader
  shpRs <- whileM (not <$> isEmpty) getShpRec
  return ShpData { shpHeader     = shpH,
              dbfFieldDescs = Nothing, -- fill using readDbfData
              shpRecs       = shpRs }

-- | Header of the shapefile
getShpHeader :: Get ShpHeader
getShpHeader = do
  fc      <- getIntBE                 -- File code
  when (fc /= 9994) (error "getShpHeader: Input file is not a SHP file")
  _       <- replicateM 5 getWord32be -- Unused data
  shpLen  <- getIntBE                 -- File length
  shpV    <- getIntLE                 -- Version (little-endian!)
  shpT    <- getShpType               -- Shape type
  shpbb   <- getShpBBox shpT          -- Bounding box
  return ShpHeader { shpFileLength = shpLen,
                shpVersion    = shpV,
                shpType       = shpT,
                shpBB         = shpbb }

-- | Get type of the record(s)
getShpType :: Get ShpType
getShpType = shpTypeFromId <$> getIntLE

-- | Read a bounding box for relevant record type.
--   Not all bounding boxes have Z/M coordinates.
getShpBBox :: ShpType -> Get ShpBBox
getShpBBox t = do
  [xMin, yMin, xMax, yMax] <- replicateM 4 getFloat64le
  [zMin, zMax]             <- let ds = replicateM 2 getFloat64le
                               in if t `elem` zTypes
                                     then map Just <$> ds
                                     else ds >> return [Nothing, Nothing]
  [mMin, mMax]             <- let ds = replicateM 2 getFloat64le
                               in if t `elem` mTypes
                                     then map Just <$> ds
                                     else ds >> return [Nothing, Nothing]
  return ShpBBox { shpXMin = xMin,
              shpXMax = xMax,
              shpYMin = yMin,
              shpYMax = yMax,
              shpZMin = zMin,
              shpZMax = zMax,
              shpMMin = mMin,
              shpMMax = mMax }

-- | Read a single record
getShpRec :: Get ShpRec
getShpRec = do
  recNum      <- getIntBE             -- Record number
  recLen      <- getIntBE             -- Record length
  recType     <- getShpType           -- Shape type
  recContents <- if recType == ShpNull -- Record contents
                    then return Nothing
                    else Just <$> getRecContents recType
  return ShpRec { shpRecNum      = recNum,
             shpRecLen      = recLen,
             shpRecContents = recContents,
             shpRecLabel    = Nothing,
             shpRecType     = recType }

-- | Get record contents for record of given type
getRecContents :: ShpType -> Get RecContents
getRecContents t = case t of

        ShpPoint -> do
          p <- getPoint
          return $ RecPoint p

        ShpPointM -> do
          p <- getPoint
          m <- getFloat64le
          return $ RecPointM { pmPoint = p, pmM = m }

        ShpPointZ -> do
          p <- getPoint
          z <- getFloat64le
          m <- getFloat64le
          return $ RecPointZ { pzPoint = p, pzZ = z, pzM = m }

        ShpMultiPoint -> do
          (bb, nPoints, points) <- getPointsData
          return RecMultiPoint { recMPBBox      = bb,
                            recMPNumPoints = nPoints,
                            recMPPoints    = points }

        ShpMultiPointM -> do
          (bb, nPoints, points) <- getPointsData
          (mMin, mMax, ms)      <- getMData nPoints
          return RecMultiPointM { recMPMBBox      = bb,
                             recMPMNumPoints = nPoints,
                             recMPMPoints    = points,
                             recMPMMRange    = (mMin, mMax),
                             recMPMMs        = ms}

        ShpMultiPointZ -> do
          (bb, nPoints, points) <- getPointsData
          (zMin, zMax, zs)      <- getZData nPoints
          (mMin, mMax, ms)      <- getMData nPoints
          return RecMultiPointZ { recMPZBBox      = bb,
                             recMPZNumPoints = nPoints,
                             recMPZPoints    = points,
                             recMPZZRange    = (zMin, zMax),
                             recMPZZs        = zs,
                             recMPZMRange    = (mMin, mMax),
                             recMPZMs        = ms }

        ShpPolyLine -> do
          (bb, nParts, nPoints, partLengths, pntLists) <- getPolyData
          return RecPolyLine { recPolLBBox        = bb,
                          recPolLNumParts    = nParts,
                          recPolLNumPoints   = nPoints,
                          recPolLPartLengths = partLengths,
                          recPolLPoints      = pntLists }

        ShpPolyLineM -> do
          (bb, nParts, nPoints, partLengths, pntLists) <- getPolyData
          (mMin, mMax, ms)                             <- getMData nPoints
          return RecPolyLineM { recPolLMBBox        = bb,
                           recPolLMNumParts    = nParts,
                           recPolLMNumPoints   = nPoints,
                           recPolLMPartLengths = partLengths,
                           recPolLMPoints      = pntLists,
                           recPolLMMRange      = (mMin, mMax),
                           recPolLMMs          = ms }

        ShpPolyLineZ -> do
          (bb, nParts, nPoints, partLengths, pntLists) <- getPolyData
          (zMin, zMax, zs)                             <- getZData nPoints
          (mMin, mMax, ms)                             <- getMData nPoints
          return RecPolyLineZ { recPolLZBBox        = bb,
                           recPolLZNumParts    = nParts,
                           recPolLZNumPoints   = nPoints,
                           recPolLZPartLengths = partLengths,
                           recPolLZPoints      = pntLists,
                           recPolLZZRange      = (zMin, zMax),
                           recPolLZZs          = zs,
                           recPolLZMRange      = (mMin, mMax),
                           recPolLZMs          = ms }

        ShpPolygon -> do
          (bb, nParts, nPoints, partLengths, pntLists) <- getPolyData
          return RecPolygon { recPolBBox        = bb,
                         recPolNumParts    = nParts,
                         recPolNumPoints   = nPoints,
                         recPolPartLengths = partLengths,
                         recPolPoints      = pntLists }

        ShpPolygonM -> do
          (bb, nParts, nPoints, partLengths, pntLists) <- getPolyData
          (mMin, mMax, ms)                             <- getMData nPoints
          return RecPolygonM { recPolMBBox        = bb,
                          recPolMNumParts    = nParts,
                          recPolMNumPoints   = nPoints,
                          recPolMPartLengths = partLengths,
                          recPolMPoints      = pntLists,
                          recPolMMRange      = (mMin, mMax),
                          recPolMMs          = ms }

        ShpPolygonZ -> do
          (bb, nParts, nPoints, partLengths, pntLists) <- getPolyData
          (zMin, zMax, zs)                             <- getZData nPoints
          (mMin, mMax, ms)                             <- getMData nPoints
          return RecPolygonZ { recPolZBBox        = bb,
                          recPolZNumParts    = nParts,
                          recPolZNumPoints   = nPoints,
                          recPolZPartLengths = partLengths,
                          recPolZPoints      = pntLists,
                          recPolZZRange      = (zMin, zMax),
                          recPolZZs          = zs,
                          recPolZMRange      = (mMin, mMax),
                          recPolZMs          = ms }

        ShpNull ->
          return RecNull

        ShpMultiPatch ->
          error "getShpRec: MultiPatch type is not supported, sorry!"

-- | Recurring pattern of bounding box with a number of points
getPointsData :: Get (RecBBox, Int, [Point])
getPointsData = do
  bb      <- getRecBBox
  nPoints <- getIntLE
  points  <- replicateM nPoints getPoint
  return (bb, nPoints, points)

-- | Recurring pattern of a bounding box with polygon data
getPolyData :: Get (RecBBox, Int, Int, [Int], [[Point]])
getPolyData = do
  bb              <- getRecBBox
  nParts          <- getIntLE
  nPoints         <- getIntLE
  partIndices     <- replicateM nParts getIntLE
  let partLengths = steps $ partIndices ++ [nPoints]
  pntLists        <- mapM getPointList partLengths
  return (bb, nParts, nPoints, partLengths, pntLists)

-- | Recurring pattern of M-valued data
getMData :: Int -> Get (Double, Double, [Double])
getMData nPoints = do
  [mMin, mMax] <- replicateM 2 getFloat64le
  ms           <- replicateM nPoints getFloat64le
  return (mMin, mMax, ms)

-- | Recurring pattern of Z-valued data (same structure as M-valued data)
getZData :: Int -> Get (Double, Double, [Double])
getZData = getMData

-- | Record bounding boxes contain no information on M/Z
getRecBBox :: Get RecBBox
getRecBBox = do
  [xMin, yMin, xMax, yMax] <- replicateM 4 getFloat64le
  return RecBBox { recXMin = xMin,
              recXMax = xMax,
              recYMin = yMin,
              recYMax = yMax }

