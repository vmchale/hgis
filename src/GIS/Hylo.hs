module GIS.Hylo where

import Geometry.Shapefile.MergeShpDbf
import Geometry.Shapefile.Types
import Data.List
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

districtToMap :: [District] -> Map
districtToMap districts = labelledDistricts .~ dist $ def
    where dist = zip (concat $ (fmap _shape) districts) (fmap _districtLabel districts)

getDistricts :: FilePath -> IO [District]
getDistricts filepath = do
        dbfExists <- doesFileExist (stripExt filepath <> ".dbf")
        file <- readShpWithDbf filepath
        let districtLabels = fromJust $ (fmap labels) $ mapM (shpRecLabel) . shpRecs $ file -- <$> for print? 
        let shapes = (map (getPolygon . fromJust . shpRecContents)) . shpRecs $ file
        let perimeters = map (getPerimeter . getPolygon . fromJust . shpRecContents) . shpRecs $ file
        let areas = map (fmap areaPolyRectangular . getPolygon . fromJust . shpRecContents) . shpRecs $ file
        let tuple = zip4 shapes districtLabels perimeters areas
        pure $ fmap (\(a,b,c,d) -> District a b c d) tuple

getPerimeter :: [Polygon] -> Double
getPerimeter lines = sum $ fmap segmentLength lines
    where segmentLength [x1, x2]       = distance x1 x2
          segmentLength (x1:x2:points) = segmentLength (x2:points) + distance x1 x2

getPolygon :: RecContents -> [Polygon]
getPolygon (RecPolygon { recPolPoints = pt }) = pt
getPolygon (RecPolygonM { recPolMPoints = pt }) = pt
getPolygon (RecPolygonZ { recPolZPoints = pt }) = pt
