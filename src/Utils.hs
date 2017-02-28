module Utils where

labels :: (Show a) => [[a]] -> [String]
labels = map ((filter (/=' ')) . (take 35) . (drop 11) . show . (!!3))

flatten :: ((a,b),c) -> (a,b,c)
flatten ((x,y),z) = (x,y,z)
