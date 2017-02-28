module GIS.Utils where

import Data.Char

getExt :: String -> String
getExt = fmap toLower . (drop 1) . (dropWhile (/='.'))

labels :: (Show a) => [[a]] -> [String]
labels = map ((filter (/=' ')) . (take 35) . (drop 11) . show . (!!3))

flatten :: ((a,b),c) -> (a,b,c)
flatten ((x,y),z) = (x,y,z)
