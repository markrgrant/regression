module Stat where

import qualified Data.Vector as V


-- Perform simple linear regression, returning the slope and
-- intercept
slr rows xgetter ygetter =
    let xvalues   = V.map xgetter rows 
        yvalues   = V.map ygetter rows
        n         = realToFrac $ V.length xvalues
        sumx      = V.sum xvalues
        sumy      = V.sum yvalues
        sumxx     = V.sum $ V.zipWith (*) xvalues xvalues
        sumxy     = V.sum $ V.zipWith (*) xvalues yvalues
        slope     = (sumxy - sumy*sumx / n) / (sumxx - sumx*sumx / n)
        intercept = (sumy - slope*sumx) / n
    in (slope, intercept)


-- compute the residual sum of squares
rss rows xgetter ygetter slope intercept =
    let xvalues   = V.map xgetter rows 
        yvalues   = V.map ygetter rows
        ypred     = V.map (\x -> x*slope + intercept) xvalues
        rss       = V.sum $ V.zipWith (\y y' -> (y - y')**2)  yvalues ypred
    in rss
