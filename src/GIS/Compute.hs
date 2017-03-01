module GIS.Compute where

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

--doesn't use the projection tho!!
getDistricts :: FilePath -> IO [District]
getDistricts filepath = do 
        file <- readShpWithDbf filepath
        let districtLabels = fromJust $ (fmap labels) $ mapM (shpRecLabel) . shpRecs $ file -- <$> for print? 
        let shapes = (map (getPolygon . fromJust . shpRecContents)) . shpRecs $ file
        let perimeters = map (getPerimeter . getPolygon . fromJust . shpRecContents) . shpRecs $ file
        {--let areas = map (fmap areaPolygon . getPolygon . fromJust . shpRecContents) . shpRecs $ file--}
        let areas = map (fmap areaPolyRectangular . getPolygon . fromJust . shpRecContents) . shpRecs $ file
        let tuple = zip4 shapes districtLabels perimeters areas
        --let tuple = zip (concat shapes) perimeters
        --putStrLn . unlines . (map show) $ zip3 districtLabels perimeters areas
        pure $ map (\(a,b,c,d) -> District a b c d) tuple

getShapes :: FilePath -> IO [[Polygon]]
getShapes = (fmap (fmap ((project mercator) . (view shape)))) . getDistricts

getAll = (fmap concat) . getShapes

getPerimeter :: [[Point]] -> Double
getPerimeter lines = sum $ fmap segmentLength lines
    where segmentLength [x1, x2]       = distance x1 x2
          segmentLength (x1:x2:points) = segmentLength (x2:points) + distance x1 x2

getPolygon :: RecContents -> [[Point]]
getPolygon (RecPolygon { recPolPoints = pt }) = pt
getPolygon (RecPolygonM { recPolMPoints = pt }) = pt
getPolygon (RecPolygonZ { recPolZPoints = pt }) = pt
