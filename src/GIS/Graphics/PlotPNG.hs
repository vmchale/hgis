{-# LANGUAGE RankNTypes #-}

-- | Module to generate PNGs from shapefiles
module GIS.Graphics.PlotPNG where

import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo
import GIS.Types
import Data.Monoid
import Control.Monad
import GIS.Hylo
import GIS.Math.Projections
import Control.Lens
import Control.Lens.Tuple
--
import GIS.Math.Spherical
import GIS.Utils
import GIS.Graphics.Plot
import GIS.Graphics.Types
import GIS.Graphics.PlotSVG hiding (fileOptions)

-- | Default file options: PNG output and 1920x1080. To change the file size,
-- you can do e.g.
-- > fo_size .~ (640,480) $ fileOptions
fileOptions = def { _fo_size = (1920, 1080) , _fo_format = PNG }

-- | Given a `Map` write it to file, where the format is determined by the
-- extension.
mkMap :: FilePath -> Map -> IO ()
mkMap filepath map = case getExt filepath of
    "png" -> mkMapPng filepath map
    "svg" -> mkMapSVG filepath map

-- | Given a `Map` write it to file, where the format is determined by the
-- extension.
mkLensMap :: (Show a) => String -> FilePath -> Lens' District a -> [District] -> IO ()
mkLensMap title filepath lens districts = case getExt filepath of
    "png" -> makeLensMapPng title filepath lens districts
    "svg" -> makeLensMapSVG title filepath lens districts

-- | Given a `Map`, write it to file. 
mkMapPng :: FilePath -> Map -> IO ()
mkMapPng path map = do
    renderableToFile fileOptions path $ mkMapR map
    putStrLn ("...output written to " <> path)

makeLensMapPng :: (Show a) => String -> FilePath -> Lens' District a -> [District] -> IO ()
makeLensMapPng title filepath lens districts = do
    renderableToFile fileOptions filepath $ mkRenderableLens lens districts title
    putStrLn ("...output written to " <> filepath)

{--
makeMapPng' :: String -> FilePath -> [Polygon] -> IO ()
makeMapPng' = (flip zip (forever $ "")) -.** makeLabelledMapPng
--}
