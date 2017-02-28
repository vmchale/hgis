module GIS.Graphics.PlotPNG where

import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo
import GIS.Types
import Data.Monoid
import GIS.Compute
import GIS.Math.Projections
import Control.Lens
import Control.Lens.Tuple
--
import GIS.Math.Spherical
import GIS.Utils

makeLabelledMapPng :: String -> FilePath -> [(Polygon, String)] -> IO ()
makeLabelledMapPng title filepath points = do
    let pairs = fmap (flatten . (over _1 shittyCentroid)) points
    let fileOptions = def { _fo_size = (1920,1080) , _fo_format = PNG } -- or SVG if we hate freedom
    let fontStyle = font_size .~ 15 $ font_weight .~ FontWeightBold $ def
    let texts = plot_annotation_values .~ pairs 
          $ plot_annotation_style .~ fontStyle
          $ def
    toFile fileOptions filepath $ do
        layout_title .= title
        layout_plots .= [(toPlot texts)]
        plot (line "border" (map (view _1) points))
    putStrLn ("...output written to " <> filepath)


mercatorFullPng :: String -> FilePath -> FilePath -> IO ()
mercatorFullPng title outfile dbfFile = do
    points <- getShapes dbfFile
    makeMapPng title outfile (project mercator $ concat points)

-- also string should be set-able
makeMapPng :: String -> FilePath -> [Polygon] -> IO ()
makeMapPng title filepath points = do
    let fileOptions = def { _fo_size = (1920,1080) , _fo_format = PNG } -- or SVG if we hate freedom
    toFile fileOptions filepath $ do
        layout_title .= title
        plot (line "border" points)
    putStrLn ("...output written to " <> filepath)
