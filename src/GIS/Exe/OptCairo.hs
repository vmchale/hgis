module GIS.Exe.OptCairo
    ( exec
    , Program (..)
    ) where

import GIS.Compute
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
    --makeFolders
    let path = unHelpful . file $ clinput
    let outfile = unHelpful . output $ clinput
    case getExt outfile of
        "svg" -> makeMapSVG "Testfile" outfile =<< getAll path
        "png" -> makeMapPng "Testfile" outfile =<< getAll path
