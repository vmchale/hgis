-- | Miscellaneous utils for math
module GIS.Math.Utils where

import GIS.Types
import Control.Lens
import Control.Lens.Tuple

-- | Convert a `Double` from degrees to radians.
radians = (*(pi/180))

-- | Convert both coördinates to radians.
toRadians :: Point -> Point
toRadians = (over _1 radians) . (over _2 radians)
