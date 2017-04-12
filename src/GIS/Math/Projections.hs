-- | Module containing several useful projections for generating maps.
module GIS.Math.Projections where

import Control.Lens
import Control.Lens.Tuple
import GIS.Types
import GIS.Math.Utils
import GIS.Graphics.Types

-- | For use as a reference point in certain projections.
washingtonDC :: Point
washingtonDC = over _2 radians $ over _1 radians (38.9072, -77.0369)

-- | For use as a reference point with the Craig retroazimuthal projection
mecca :: Point
mecca = over _2 radians $ over _1 radians (21.3891, 39.8579)

-- | Littow retroazimuthal + conformal projection
littow :: Projection
littow (long, lat) = ((sin(long - referenceLong)/(cos lat)), (cos (long - referenceLong) * (tan lat)))
    where referenceLong = radians 0

-- | Craig retroazimuthal projection
-- (works on a subset of the world)
craig :: Point -> Projection
craig referencePoint (long, lat) = (long - referenceLong, y)
    where (referenceLong, referenceLat) = referencePoint
          y = if long - referenceLong == 0 then expr else (long - referenceLong)/(sin(long - referenceLong)) * expr
          expr = ((sin lat) * (cos (long - referenceLong)) - (tan referenceLat) * (cos lat)) 

-- | Winkel Tripel projection (standard for the National Geographic Society since 
winkel3 :: Projection
winkel3 (long, lat) = ((lambda * (cos phi1) + (2 * cos lat * sin (lambda/2)/(sinc alpha)))/2, (lat + (sin lat)/(sinc alpha))/2)
    where lambda = long - lambda0
          phi1 = acos $ 2 / pi
          alpha = acos $ cos lat * (cos (lambda/2))
          lambda0 = radians (-77.0369)

-- | Mercator projection.
mercator :: Projection
mercator (long, lat) = (long - meridian, asinh(tan(lat)))
    where meridian = radians (-98.5795)

-- | Bonne projection with standard parallel at 45 N and central meridian
-- centered at Washington DC
bonne :: Projection
bonne (long, lat) = (rho * (sin e), (cot phi1 - rho * (cos e)))
    where rho = (cot phi1) + phi1 - lat
          e = (long - meridian) * (cos lat) / rho
          phi1 = radians 45 -- standard parallel @ 45 N 
          meridian = radians (-77.0369) -- central meridian @ dc
          cot = (1/) . tan

-- | Albers projection for a given reference point. To make it usable you can
-- use
-- > ablers washingtonDC
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

-- | Helper to project given a `Polygon`.
project :: Projection -> Polygon -> Polygon
project f = fmap (f . toRadians)

-- | Helper to apply a projection given a `Map`.
projectMap :: Projection -> Map -> Map
projectMap p = over (labelledDistricts) (fmap (over _1  (fmap (project p))))
