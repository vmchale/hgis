module GIS.Graphics.PlotSVG where

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams 
import GIS.Compute
import GIS.Types
import Data.Monoid
import GIS.Math.Projections

mercatorFullSVG :: String -> FilePath -> FilePath -> IO ()
mercatorFullSVG title outfile dbfFile = do
    points <- getShapes dbfFile
    makeMapSVG title outfile (project mercator $ concat points)

-- also something for svg etc.
makeMapSVG :: String -> FilePath -> [Polygon] -> IO ()
makeMapSVG title filepath points = do
    let fileOptions = def { _fo_size = (1920,1080) , _fo_format = SVG } -- or SVG if we hate freedom
    toFile fileOptions filepath $ do
        layout_title .= title
        plot (line "border" points)
    putStrLn ("...output written to " <> filepath)
