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
    where distA (District _ label _ area _) = (label, sum area) -- TODO figure out which one is the correct one

-- should mention it's km
-- | Get the perimeters of various objects and return a string suitable for printing
districtPerimeter :: [District] -> String
districtPerimeter districts = concat . intercalate (pure "\n") $ map (pure . show . distP) districts
    where distP (District _ label perimeter _ _) = (label, perimeter)

-- | Label with relative compactness
districtCompactness :: [District] -> String
districtCompactness districts = concat . intercalate (pure "\n") $ map (pure . show . distC) districts
    where distC (District _ label _ _ compacticity) = (label, compacticity)

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
districtToMapFiles = map (\(District polygons label _ area _) -> title .~ label ++ "-" ++ (show . sum $ area) $ labelledDistricts .~ (zip polygons (nullLabel polygons)) $ def)
    where nullLabel polys = map (const "") [1..(length polys)]

-- | Given the path to a shapefile, return a list of districts
getDistricts :: FilePath -> IO [District]
getDistricts filepath = do
        dbfExists <- doesFileExist (stripExt filepath <> ".dbf")
        file <- if dbfExists then readShpWithDbf filepath else readShpFile filepath
        let districtLabels = fromJust $ (fmap labels) $ mapM (shpRecLabel) . shpRecs $ file -- <$> for print? 
        let shapes = (map (getPolygon . fromJust . shpRecContents)) . shpRecs $ file
        let perimeters = map (totalPerimeter . getPolygon . fromJust . shpRecContents) . shpRecs $ file
        let areas = map (fmap areaPolygon . getPolygon . fromJust . shpRecContents) . shpRecs $ file
        let compacticity = map (relativeCompactness . concat . getPolygon . fromJust . shpRecContents) . shpRecs $ file
        pure $ zipWith5 (\a b c d e -> District a b c d e) shapes districtLabels perimeters areas compacticity
        --let tuple = zip5 shapes districtLabels perimeters areas compactness
        --pure $ fmap (\(a,b,c,d,e) -> District a b c d e) tuple

-- | Helper function for extracting from shapefiles.
getPolygon :: RecContents -> [Polygon]
getPolygon (RecPolygon { recPolPoints = pt }) = pt
getPolygon (RecPolygonM { recPolMPoints = pt }) = pt
getPolygon (RecPolygonZ { recPolZPoints = pt }) = pt
