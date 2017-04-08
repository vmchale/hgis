{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Module containing types for various geometrical objects. Lenses included. 
module GIS.Types where

import Text.PrettyPrint.ANSI.Leijen
import GHC.Generics
import Control.Lens

type Point = (Double, Double)
type Polygon = [Point]
type Projection = (Double, Double) -> (Double, Double)

-- | Data type for one record in a shape file, also capable of storing basic
-- information about the district. 
data District = District { _shape         :: [Polygon]
                         , _districtLabel :: String
                         , _perimeter     :: Double
                         , _area          :: [Double]
                         , _compactness   :: Double
                         } deriving (Generic, Show)

makeLenses ''District

-- possibly get rid of this idk
data DbfReadError = NotAPolygon | ShpNull

instance Show DbfReadError where
    show NotAPolygon = show $ red (text "Error: ") <> (text "Shape not a polygon! Are you sure you're opening a district?")
    show ShpNull = show $ red (text "Error: ") <> (text "")
