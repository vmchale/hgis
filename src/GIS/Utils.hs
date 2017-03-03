-- | Miscellaneous utils and helper functions.
module GIS.Utils where

import Data.Char

-- | Strip extension from a filepath
stripExt :: String -> String
stripExt = reverse . drop 1 . dropWhile (/='.') . reverse

-- | Get extension given a filepath
getExt :: String -> String
getExt = fmap toLower . reverse . (takeWhile (/='.')) . reverse

-- | Make sure labels on shapefile objects are suitable for human reading.
labels :: (Show a) => [[a]] -> [String]
labels = map ((filter (/=' ')) . (take 35) . (drop 11) . show . (!!3))

-- | Flatten a nested tuple.
flatten :: ((a,b),c) -> (a,b,c)
flatten ((x,y),z) = (x,y,z)
