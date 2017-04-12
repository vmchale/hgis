{-# LANGUAGE RankNTypes #-}

-- | Module containing renderable objects and plots for our maps
module GIS.Graphics.Plot where

import GIS.Types
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Easy
import GIS.Math.Spherical
import GIS.Utils
import Data.Colour.Names
import GIS.Graphics.Types
import GIS.Math.Projections

-- | Given a map, return a `Renderable ()` for use with the
-- """Graphics.Rendering.Char""" module. 
mkMapR :: Map -> Renderable ()
mkMapR (Map { _projection = p , _title = tit , _labelEntities = le , _labelledDistricts = points }) = case le of
    True -> mkRenderableLabelled tit points
    False -> mkRenderableLabelled tit $ fmap (over _2 (const "")) points

mkRenderablePlots :: String -> [Plot Double Double] -> Renderable ()
mkRenderablePlots title plots = toRenderable $
    layout_title .~ title $ layout_plots .~ plots $ def

mkRenderableLens :: (Show a) => Lens' District a -> [District] -> String -> Renderable ()
mkRenderableLens lens districts title = mkRenderableLabelled title (labelByLens lens districts)

mkRenderableLabelled :: String -> [([Polygon], String)] -> Renderable ()
mkRenderableLabelled title points = mkRenderablePlots title [ plotDataPoints (concat . (fmap fst) $ points), plotLabels points ]

-- | Helper function to plot data points as appropriate for a map, i.e. using
-- contiguous lines. 
plotDataPoints :: [Polygon] -> Plot Double Double
plotDataPoints points = toPlot $
    plot_lines_values .~ points $ plot_lines_style . line_color .~ opaque blue $ plot_lines_title .~ "Border" $ def

-- | Set labels via a lens field.
labelByLens :: (Show a) => Lens' District a -> [District] -> [([Polygon], String)]
labelByLens lens districts = zip (fmap (view shape) districts) (fmap (show . (view lens)) districts)

-- | Helper function to plot labels on a map. The ordinate will be plotted at
-- the centroid of the abscissa, which may be outside the polygon if it is
-- concave. 
plotLabels :: [([Polygon], String)] -> Plot Double Double
plotLabels points = toPlot texts
    where pairs = fmap (flatten . (over _1 (shittyCentroid . concat))) points
          fontStyle = font_size .~ 15 $ font_weight .~ FontWeightBold $ def
          texts = plot_annotation_values .~ pairs $ plot_annotation_style .~ fontStyle $ def
