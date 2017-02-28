{-|
Module      : Geometry.Shapefile.Internal
Description : Some functions for reuse inside the library
Author      : Sam van Herwaarden <samvherwaarden@gmail.com>
-}

module Geometry.Shapefile.Internal where

import Control.Monad ( replicateM )
import Data.Binary.Get
import Data.Binary.IEEE754 ( getFloat64le )
import qualified Data.ByteString.Char8 as C

-- | 8-bit Int value
getInt8 :: Get Int
getInt8 = fromIntegral <$> getWord8

-- | 32-bit big-endian Int
getIntBE :: Get Int
getIntBE = fromIntegral <$> getWord32be

-- | 16-bit big-endian Int
getInt16BE :: Get Int
getInt16BE = fromIntegral <$> getWord16be

-- | 32-bit little-endian Int
getIntLE :: Get Int
getIntLE = fromIntegral <$> getWord32le

-- | 16-bit little-endian Int
getInt16LE :: Get Int
getInt16LE = fromIntegral <$> getWord16le

-- | String of length `l`
getString :: Int -> Get String
getString l = filter (/= '\NUL') . C.unpack <$> getByteString l

-- | Char (presumably 8-bit)
getCharVal :: Get Char
getCharVal = C.head <$> getByteString 1

type Point = (Double, Double)

-- | Two doubles making up a point with two coordinates
getPoint :: Get Point
getPoint = do x <- getFloat64le
              y <- getFloat64le
              return (x, y)

-- | Get the increments between list values
steps :: Num a => [a] -> [a]
steps         [] = []
steps     (_:[]) = []
steps (x1:x2:xs) = x2 - x1 : steps (x2:xs)

-- | List of points of length `numPoints`
getPointList :: Int -> Get [Point]
getPointList numPoints = replicateM numPoints getPoint
