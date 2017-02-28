-- | Note you can use `id` to make a null projection: this isn't great but it can help you debug
module Math.Projections where

import Control.Lens
import Control.Lens.Tuple
import Types
import Math.Utils

washingtonDC :: Point
washingtonDC = over _2 radians $ over _1 radians (38.9072, -77.0369)

mercator :: Projection
mercator (long, lat) = (long - meridian, asinh(tan(lat)))
    where meridian = radians (-98.5795)

bonne :: Projection
bonne (long, lat) = (rho * (sin e), (cot phi1 - rho * (cos e)))
    where rho = (cot phi1) + phi1 - lat
          e = (long - meridian) * (cos lat) / rho
          phi1 = radians 45 -- standard parallel @ 45 N 
          meridian = radians (-77.0369) -- central meridian @ dc
          cot = (1/) . tan

albers :: Point -> Projection
albers referencePoint (long, lat) = (rho * (sin theta), rho' - rho * (cos theta))
    where n = (sin phi1 + sin phi2)/2
          theta = n * (long - referenceLong)
          c = (cos phi1)^2 + 2 * n * (sin phi1)
          rho = sqrt (c - 2 * n * (sin lat)) / n
          rho' = sqrt (c - 2 * n * (sin referenceLat)) / n
          phi1 = (radians 20) -- standard parallels @ 20, 50 degrees
          phi2 = (radians 50)
          (referenceLong, referenceLat) = referencePoint 

project :: Projection -> [Polygon] -> [Polygon]
project f = fmap $ fmap (f . toRadians)
