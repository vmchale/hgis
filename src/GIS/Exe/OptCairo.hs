module GIS.Exe.OptCairo
    ( exec
    , Program (..)
    ) where

import GIS.Hylo
import Control.Lens
import Control.Monad
import Options.Generic hiding (getAll)
import Control.Monad.IO.Class
import Data.Monoid hiding (getAll)
import System.Directory
--
import GIS.Utils
import GIS.Math.Projections
import GIS.Math.Spherical
import GIS.Graphics.PlotSVG
import GIS.Graphics.PlotPNG
import GIS.Types
import GIS.Exe.Opt hiding (exec)

exec :: IO ()
exec = do
    clinput <- getRecord "GIS cli utility for haskell"
    let path = unHelpful . file $ clinput
    let outfile = unHelpful . output $ clinput
    let p = pickProjection . unHelpful . projection $ clinput
    --case getExt outfile of
    --    "svg" -> mkMapSVG outfile =<< districtToMap <$> getDistricts path
    --    "png" -> mkMapPng outfile =<< districtToMap <$> getDistricts path
    case getExt outfile of
        "svg" -> mkMapSVG outfile =<< districtToMapP p <$> getDistricts path
        "png" -> mkMapPng outfile =<< districtToMapP p <$> getDistricts path
