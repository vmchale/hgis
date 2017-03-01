module GIS.Graphics.PlotPNG where

import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo
import GIS.Types
import Data.Monoid
import Data.Function.Contravariant.Syntax
import Control.Monad
import GIS.Compute
import GIS.Math.Projections
import Control.Lens
import Control.Lens.Tuple
--
import GIS.Math.Spherical
import GIS.Utils
import GIS.Graphics.Plot

makeLabelledMapPng :: String -> FilePath -> [(Polygon, String)] -> IO ()
makeLabelledMapPng title filepath points = do
    let fileOptions = def { _fo_size = (1920, 1080) , _fo_format = PNG }
    toFile fileOptions filepath $ do
        layout_title .= title
        layout_plots .= [plotLabels points]
        plot (line mempty (map (view _1) points))
    putStrLn ("...output written to " <> filepath)

-- | Make a make from a file, with the mercator projection. 
-- projectFullPng :: Projection -> String -> FilePath -> FilePath -> IO ()

makeMapPng :: String -> FilePath -> [Polygon] -> IO ()
makeMapPng = (flip zip (forever $ "")) -.** makeLabelledMapPng
