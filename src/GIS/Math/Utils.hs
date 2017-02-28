module GIS.Math.Utils where

import GIS.Types
import Control.Lens
import Control.Lens.Tuple

radians = (*(pi/180))

toRadians :: Point -> Point
toRadians = (over _1 radians) . (over _2 radians)
