{-# LANGUAGE OverloadedStrings #-}


import Stat
import House

import Data.Csv
import Regression
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as B
import qualified Data.Vector as V

extraFeatures = [("bedrooms_squared", (^2) . bedrooms),
                  ("bed_bath_rooms", \r -> bedrooms r * bathrooms r),
                  ("log_sqft_living", log . sqft_living),
                  ("lat_plus_long", \r -> lat r + long r)]


features_1 = [
           ("intercept", const 1),
           ("sqft_living", sqft_living),
           ("bedrooms", bedrooms),
           ("bathrooms", bathrooms),
           ("lat", lat),
           ("long", long)]
features_2 = features_1 ++ [extraFeatures !! 1]
features_3 = features_1 ++ extraFeatures

main :: IO ()
main = do
    [trainingFile, testFile] <- getArgs
    trainingData <- B.readFile trainingFile
    testData <- B.readFile testFile

    let trainingCSV = decodeByName trainingData :: Either String (Header, V.Vector House)
        testCSV = decodeByName testData :: Either String (Header, V.Vector House)

        output = ("price", price)
    case trainingCSV of
        Left err -> putStrLn $ "error reading " ++ trainingFile
        Right (trainingHeader, trainingHouses) -> 
            case testCSV of
                Left err -> putStrLn $ "error reading " ++ testFile
                Right (testHeader, testHouses) -> do
                    let training_inputs = V.toList trainingHouses
                        test_inputs = V.toList testHouses
                        h = feature_matrix test_inputs extraFeatures
                        test_feature_names = map fst extraFeatures
                        test_feature_means = map (feature_mean h) test_feature_names 
                        model1 = create test_inputs features_1 output
                        model2 = create test_inputs features_2 output
                        model3 = create test_inputs features_3 output
                    putStrLn "test feature means:"
                    print $ zip test_feature_names test_feature_means
                    putStrLn $ "model1 weights:\n" ++ (show (model_weights model1))
                    putStrLn $ "model2 weights:\n" ++ (show (model_weights model2))
                    putStrLn $ "model3 weights:\n" ++ (show (model_weights model3))
                    putStrLn $ "model1 rss = " ++ (show (model_rss model1))
                    putStrLn $ "model2 rss = " ++ (show (model_rss model2))
                    putStrLn $ "model3 rss = " ++ (show (model_rss model3))
