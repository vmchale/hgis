module GIS.Exe.Opt where

import GIS.Hylo
import Control.Monad
import Control.Lens hiding (argument)
import Options.Applicative
import Control.Monad.IO.Class
import Data.Monoid hiding (getAll)
import System.Directory
--
import GIS.Utils
import GIS.Math.Projections
import GIS.Math.Spherical
import GIS.Graphics.PlotSVG
import GIS.Types
import GIS.Exe.Parser
import GIS.Graphics.Types

pick :: Program -> IO ()
pick (Program (MapMaker outfile False projection) infile) = let p = pickProjection projection in
    mkMapSVG outfile =<< districtToMapP p <$> getDistricts infile -- svg now idk?
pick (Program (MapMaker outfile True projection) infile) = let p = pickProjection projection in
    makeFoldersSVG =<< districtToMapFilesP p <$> getDistricts infile

exec :: IO ()
exec = pick =<< execParser helpDisplay

makeFoldersSVG :: [Map] -> IO ()
makeFoldersSVG maps = do
    createDirectoryIfMissing False "maps"
    mapM_ (\m -> mkMapSVG ("maps/" <> (view title m) <> ".svg") m) maps
