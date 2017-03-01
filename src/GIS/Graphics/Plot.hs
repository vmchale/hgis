module GIS.Graphics.Plot where

import GIS.Types
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Easy
import GIS.Math.Spherical
import GIS.Utils
import Data.Colour.Names
import GIS.Graphics.Types
import GIS.Math.Projections

mkMap :: Map -> Renderable ()
mkMap (Map { _projection = p , _title = tit , _labelEntities = le , _labelledDistricts = points }) = case le of
    True -> mkRenderableLabelled tit points
    False -> mkRenderableLabelled tit $ fmap (over _2 (const "")) points

mkRenderablePlots :: String -> [Plot Double Double] -> Renderable ()
mkRenderablePlots title plots = toRenderable $
    layout_title .~ title $ layout_plots .~ plots $ def

mkRenderableLabelled :: String -> [(Polygon, String)] -> Renderable ()
mkRenderableLabelled title points = mkRenderablePlots title [ plotDataPoints (map fst points), plotLabels points ]

plotDataPoints :: [Polygon] -> Plot Double Double
plotDataPoints points = toPlot $
    plot_lines_values .~ points $ plot_lines_style . line_color .~ opaque blue $ plot_lines_title .~ "Border" $ def

plotLabels :: [(Polygon, String)] -> Plot Double Double
plotLabels points = toPlot texts
    where pairs = fmap (flatten . (over _1 shittyCentroid)) points
          fontStyle = font_size .~ 15 $ font_weight .~ FontWeightBold $ def
          texts = plot_annotation_values .~ pairs $ plot_annotation_style .~ fontStyle $ def
