module FeatureChart where

import qualified Data.Vector as V
import Data.Csv hiding ((.=))
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Easy hiding (view)
import Graphics.Rendering.Chart.Grid
import Graphics.Rendering.Chart.Backend.Cairo (renderableToFile)



-- A chart for categorical data that is an instance of PlotValue
customChart featureName rows xgetter ygetter = layout
    where
        xvals = V.map xgetter rows
        yvals = V.map ygetter rows
        pairs = V.toList $ V.zipWith (,) xvals yvals
        plotData = plot_points_style .~ filledCircles 10 (opaque red)
            $ plot_points_values .~ pairs
            $ plot_points_title .~ featureName
            $ def

        layout = layout_title .~ featureName
            $ layout_plots .~ [toPlot plotData]
            $ def


-- A single scatterplot  of a feature for the grid
featureChart :: (PlotValue b, Show c, RealFloat c, PlotValue c) => String -> V.Vector a -> (a -> b) -> (a -> c) -> Layout b c
featureChart featureName rows xgetter ygetter = execEC $ do
    layout_y_axis . laxis_generate .= scaledAxis def (miny, maxy)
    plot $ points label pairs
  where
    xvals = V.map xgetter rows
    yvals = V.map ygetter rows
    pairs = V.toList $ V.zipWith (,) xvals yvals
    label = featureName
    miny = V.minimum yvals
    --miny = 0
    maxy = V.maximum yvals
    --maxy = 100

-- Construct a grid of charts
featureGrid chartRows output titleText = title `wideAbove` aboveN [ besideN [ layoutToGrid chart | chart <- chartRow ] | chartRow <- chartRows ]
  where
    title = setPickFn nullPickFn $ label fontinfo HTA_Centre VTA_Centre titleText
    fontinfo = def { _font_size   = 15 , _font_weight = FontWeightBold }


