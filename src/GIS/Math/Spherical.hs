-- | Utilities to compute area, perimeter, etc. on the surface of a sphere.
module GIS.Math.Spherical where

import Control.Lens
import Control.Lens.Tuple
import Data.Function
import Data.List
import GIS.Types
import Data.Composition
import GIS.Math.Projections
import GIS.Math.Utils

-- | averages the coördinates of a polygon, returning a point.
shittyCentroid :: Polygon -> Point
shittyCentroid poly = (avg $ map fst poly, avg $ map snd poly)

-- | Average over a foldable container
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

-- mandelbrot/fractal dimension? 
-- consider "area of largest circumscribable circle" as well. 

-- | Compute the area of a convex polygon on the surface of a sphere.
areaConvex :: Polygon -> Double
areaConvex (base1:base2:pts) = (view _1) $ foldr stepArea (0,base2) pts
    where stepArea point (sum, base) = (sum + (areaTriangle base1 base point), point)

-- | Uses areal projection; then finds area of the polygon. 
-- WARNING: not yet to scale. 
areaPolygon :: Polygon -> Double
areaPolygon = {--(*((6371*pi)^2)) . --}areaPolyRectangular . (fmap bonne)

-- | Find the area of a polygon with rectangular coördinates given. 
areaPolyRectangular :: Polygon -> Double
areaPolyRectangular (pt:pts) = 0.5 * (fst (foldl' areaPolyCalc (0,pt) pts))
    where areaPolyCalc (sum,(x,y)) (xNext,yNext) = (sum + (x * yNext - xNext * y),(xNext,yNext))

-- | Distance in kilometers between two points given in degrees. 
distance :: (Double, Double) -> (Double, Double) -> Double
distance = (*6371) .* (on centralAngle toRadians)

-- | Compute central angle from points given in radians
centralAngle :: (Double, Double) -> (Double, Double) -> Double
centralAngle (long1, lat1) (long2, lat2) = centralAngle
    where centralAngle = acos $ (sin lat1) * (sin lat2) + (cos lat1) * (cos lat2) * (cos (long1 - long2))
