{-# LANGUAGE OverloadedStrings #-}


import Stat
import House

import Data.Csv
import GHC.Exts (sortWith)
import Regression.Repa
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as B
import qualified Data.Vector as V


main :: IO ()
main = do
    [filename] <- getArgs
    fileData <- B.readFile filename
    let csv = decodeByName fileData :: Either String (Header, V.Vector House)
    case csv of
        Left err -> putStrLn $ "error reading " ++ filename
        Right (header, houses) ->  do
            let inputs = V.toList houses
                output = ("price", price)
                features = [("w0", const 1), ("sqft_living", sqft_living)]
                fmat = create_features features inputs
                e = 3
                n = 0.000000000002
                initial_weights = create_weights fmat [0,0..0]
                optimizer = gradient_descent e n initial_weights
                model = create_model inputs features output optimizer
                weights = model_weights model
            putStrLn $ "model weights:\n" ++ show (model_weights model)
            putStrLn $ "model rss: " ++ show (model_rss model)
