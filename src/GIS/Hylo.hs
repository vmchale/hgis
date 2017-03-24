-- | Module to transform shapefiles.
module GIS.Hylo where

import Geometry.Shapefile.MergeShpDbf
import Geometry.Shapefile.ReadShp
import Geometry.Shapefile.Types
import Data.List
import Data.Char
import Data.Maybe
import Data.Monoid
import GIS.Math.Projections
import GIS.Math.Spherical
import GIS.Utils
import Control.Lens.Tuple
import Control.Lens
import Control.Monad
import GIS.Types
import GIS.Graphics.Types
import Data.Default
import System.Directory

-- | Get the areas of various objects and return a string suitable for printing
districtArea :: [District] -> String
districtArea districts = concat . intercalate (pure "\n") $ map (pure . show . distA) districts
    where distA (District _ label _ area) = (label, sum area) -- TODO figure out which one is the correct one

-- should mention it's km
-- | Get the perimeters of various objects and return a string suitable for printing
districtPerimeter :: [District] -> String
districtPerimeter districts = concat . intercalate (pure "\n") $ map (pure . show . distP) districts
    where distP (District _ label perimeter _) = (label, perimeter)

-- | Given a projection and lists of districts, draw a map.
districtToMapP :: Projection -> [District] -> Map
districtToMapP p = projectMap p . districtToMap

-- | Given a list of districts, draw a map.
districtToMap :: [District] -> Map
districtToMap districts = labelledDistricts .~ dist $ def
    where dist = gc $ zip (fmap _shape districts) (fmap _districtLabel districts)
          gc = concatMap (\(a,b) -> zip a (replicate (length a) b))

-- | Given a projection and list of districts, return a list of maps.
districtToMapFilesP :: Projection -> [District] -> [Map]
districtToMapFilesP p = fmap (projectMap p) . districtToMapFiles

-- | Given a list of districts, return a list of maps.
districtToMapFiles :: [District] -> [Map]
districtToMapFiles = map (\(District polygons label _ area) -> title .~ label ++ "-" ++ (show . sum $ area) $ labelledDistricts .~ (zip polygons (nullLabel polygons)) $ def)
--districtToMapFiles = map (\(District polygons label _ _) -> title .~ label $ labelledDistricts .~ (zip polygons (nullLabel polygons)) $ def)
    where nullLabel polys = map (const "") [1..(length polys)]

-- | Given the path to a shapefile, return a list of districts
getDistricts :: FilePath -> IO [District]
getDistricts filepath = do
        dbfExists <- doesFileExist (stripExt filepath <> ".dbf")
        file <- if dbfExists then readShpWithDbf filepath else readShpFile filepath
        let districtLabels = fromJust $ (fmap labels) $ mapM (shpRecLabel) . shpRecs $ file -- <$> for print? 
        let shapes = (map (getPolygon . fromJust . shpRecContents)) . shpRecs $ file
        let perimeters = map (getPerimeter . getPolygon . fromJust . shpRecContents) . shpRecs $ file
        let areas = map (fmap areaPolygon . getPolygon . fromJust . shpRecContents) . shpRecs $ file
        let tuple = zip4 shapes districtLabels perimeters areas
        pure $ fmap (\(a,b,c,d) -> District a b c d) tuple

-- | Given a list of polygons, return the total area.
getPerimeter :: [Polygon] -> Double
getPerimeter lines = sum $ fmap segmentLength lines
    where segmentLength [x1, x2]       = distance x1 x2
          segmentLength (x1:x2:points) = segmentLength (x2:points) + distance x1 x2

-- | Helper function for extracting from shapefiles.
getPolygon :: RecContents -> [Polygon]
getPolygon (RecPolygon { recPolPoints = pt }) = pt
getPolygon (RecPolygonM { recPolMPoints = pt }) = pt
getPolygon (RecPolygonZ { recPolZPoints = pt }) = pt
