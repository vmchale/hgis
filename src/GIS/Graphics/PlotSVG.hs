module GIS.Graphics.PlotSVG where

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams 
import Data.Function.Contravariant.Syntax
import Control.Monad
import GIS.Compute
import GIS.Types
import Data.Monoid
import GIS.Math.Projections
import GIS.Graphics.Plot

fileOptions = def { _fo_size = (1920, 1080) , _fo_format = SVG }

makeLabelledMapSVG :: String -> FilePath -> [(Polygon, String)] -> IO ()
makeLabelledMapSVG title filepath points = do
    renderableToFile fileOptions filepath $ mkRenderableLabelled title points
    putStrLn ("...output written to " <> filepath)

makeMapSVG :: String -> FilePath -> [Polygon] -> IO ()
makeMapSVG = (flip zip (forever $ "")) -.** makeLabelledMapSVG
