{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}

module Types where

import Text.PrettyPrint.ANSI.Leijen
import GHC.Generics
import Control.Lens

type Point = (Double, Double)
type Polygon = [Point]
type Projection = (Double, Double) -> (Double, Double)

data District = District { _shape         :: [Polygon]
                         , _districtLabel :: String
                         , _perimeter     :: Double
                         , _area          :: [Double]
                         } deriving (Generic, Show)

makeLenses ''District

-- possibly get rid of this idk
data DbfReadError = NotAPolygon | ShpNull

instance Show DbfReadError where
    show NotAPolygon = show $ (red $ text "Error: ") <> (text "Shape not a polygon! Are you sure you're opening a district?")
    show ShpNull = show $ (red $ text "Error: ") <> (text "")
    
