{-# LANGUAGE TemplateHaskell #-}

-- | Module for types associated with generating maps. Includes lenses. 
module GIS.Graphics.Types where

import Data.Default
import GIS.Types
import Control.Lens

-- | Data type for a map
data Map = Map { _projection :: Projection
               , _title :: String
               , _labelEntities :: Bool -- whether to label districts
               , _labelledDistricts :: [(Polygon, String)] -- the data we actually want to map
               --technically just needs to be a 
               --though for that matter then projection should also have a typeclass!
               }

makeLenses ''Map

instance Default Map where
    def = Map { _projection = id , _title = mempty , _labelEntities = False , _labelledDistricts = mempty }
