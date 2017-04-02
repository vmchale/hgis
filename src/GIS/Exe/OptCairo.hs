-- | This contains the main program executable for the program when built with
-- the Cairo libary
module GIS.Exe.OptCairo
    ( -- * Data types
    Program (..)
    -- * Functions
    , exec
    ) where

import GIS.Hylo
import Control.Lens
import Control.Monad
import Options.Applicative
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
import GIS.Exe.Opt hiding (exec, pick)
import GIS.Exe.Parser
import GIS.Graphics.Types

-- | Main executable; parses command line options and runs program
exec :: IO ()
exec = pick =<< execParser helpDisplay

-- | Execute the program with parsed command-line input
pick :: Program -> IO ()
pick (Program (MapMaker outfile False projection) infile) = let p = pickProjection projection in
    mkMap outfile =<< districtToMapP p <$> getDistricts infile -- svg now idk?
pick (Program (MapMaker outfile True projection) infile) = let p = pickProjection projection in
    makeFoldersPng =<< districtToMapFilesP p <$> getDistricts infile
pick (Program (Computation comp Nothing) infile) = --slightly wrong but eh. 
    case comp of
        "perimeter" -> putStrLn =<< districtPerimeter <$> getDistricts infile
        "area" -> putStrLn =<< districtArea <$> getDistricts infile
        "compactness" -> putStrLn =<< districtCompactness <$> getDistricts infile
        _ -> putStrLn "computation not recognized"

-- | Make maps as png files.
makeFoldersPng :: [Map] -> IO ()
makeFoldersPng maps = do
    createDirectoryIfMissing False "maps"
    mapM_ (\m -> mkMapPng ("maps/" <> (view title m) <> ".png") m) maps
