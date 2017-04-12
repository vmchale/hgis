{-# LANGUAGE RankNTypes #-}

-- | Module to generate SVGs from shapefiles.
module GIS.Graphics.PlotSVG where

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams 
import Control.Monad
import GIS.Hylo
import GIS.Types
import Data.Monoid
import GIS.Math.Projections
import GIS.Graphics.Plot
import GIS.Graphics.Types

-- | Default file options: PNG output and 1920x1080. To change the file size,
-- you can do e.g.
-- > fo_size .~ (640,480) $ fileOptions
fileOptions = def { _fo_size = (1920, 1080) , _fo_format = SVG }

-- | Given a `Map`, write it to file as an SVG.
mkMapSVG :: FilePath -> Map -> IO ()
mkMapSVG path map = do
    renderableToFile fileOptions path $ mkMapR map
    putStrLn ("...output written to " <> path)

makeLensMapSVG :: (Show a) => String -> FilePath -> Lens' District a -> [District] -> IO ()
makeLensMapSVG title filepath lens districts = do
    renderableToFile fileOptions filepath $ mkRenderableLens lens districts title
    putStrLn ("...output written to " <> filepath)
