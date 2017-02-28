module Math.Spherical where

import Control.Lens
import Control.Lens.Tuple
import Data.Function
import Data.List
import Types
import Data.Composition
import Math.Projections
import Math.Utils

-- use projection ahead of time so it works nicely
-- idc about much besides labelling tbh
shittyCentroid :: Polygon -> Point
shittyCentroid poly = (avg $ map fst poly, avg $ map snd poly)

avg :: (RealFrac a, Foldable t) => t a -> a
avg list = sum list / (fromIntegral . length $ list)

-- | Compute the area of a triangle using L'Huillier's formula
areaTriangle x1 x2 x3 = r^2 * e
    where r = 6371
          e = 4 * atan(sqrt(tan(s/2) * tan((s - a)/2) * tan((s - b)/2) * tan((s - c)/2)))
          s = (a + b + c) / 2
          a = distanceRad x1 x2
          b = distanceRad x1 x3
          c = distanceRad x2 x3
          distanceRad = on centralAngle toRadians

-- wait jk this only works on convex polygons
areaConvex :: Polygon -> Double
areaConvex (base1:base2:pts) = (view _1) $ foldr stepArea (0,base2) pts
    where stepArea point (sum, base) = (sum + (areaTriangle base1 base point), point)

-- hawaii yields: -0.6010684007833547
-- should be: 28,311 km^2
-- montana: 18.707615797916404
-- 381,154
-- south dakota: 199,900 km^2 / -13.273195731718289
-- ok sooo it's working-ish with areaPolyReactangular? 
areaPolygon :: Polygon -> Double
areaPolygon = {--(*((6371*pi)^2)) . --}areaPolyRectangular . (fmap bonne)

areaPolyRectangular :: Polygon -> Double
areaPolyRectangular (pt:pts) = 0.5 * (fst (foldl' areaPolyCalc (0,pt) pts))
    where areaPolyCalc (sum,(x,y)) (xNext,yNext) = (sum + (x * yNext - xNext * y),(xNext,yNext))

-- | Distance in kilometers
distance :: (Double, Double) -> (Double, Double) -> Double
distance = (*6371) .* (on centralAngle toRadians)

-- | Compute central angle from points given in radians
centralAngle :: (Double, Double) -> (Double, Double) -> Double
centralAngle (long1, lat1) (long2, lat2) = centralAngle
    where centralAngle = acos $ (sin lat1) * (sin lat2) + (cos lat1) * (cos lat2) * (cos (long1 - long2))
