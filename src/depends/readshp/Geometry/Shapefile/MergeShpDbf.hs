{-|
Module      : Geometry.Shapefile.MergeShpDbf
Description : Code for reading ESRI shapefiles with DBF data
Author      : Sam van Herwaarden <samvherwaarden@gmail.com>
-}

module Geometry.Shapefile.MergeShpDbf ( readShpWithDbf ) where

import System.FilePath

import qualified Data.ByteString.Lazy as BL

import Geometry.Shapefile.ReadDbf
import Geometry.Shapefile.ReadShp
import Geometry.Shapefile.Types

-- | Read a shp-file from `fp`, and a dbf file from the same path but with
--   modified extension.
readShpWithDbf :: String -> IO ShpData
readShpWithDbf fp = do
  shpData <- readShpData <$> BL.readFile fp
  let dbfPath = dropExtension fp ++ ".dbf"
  dbfData <- readDbfData <$> BL.readFile dbfPath
  return shpData {
      dbfFieldDescs = Just $ dbfFields dbfData,
      shpRecs = zipWith addRecLabel (shpRecs shpData) (dbfRecords dbfData)
    }

addRecLabel :: ShpRec -> [DbfRecord] -> ShpRec
addRecLabel sr labels = sr { shpRecLabel = Just labels }
