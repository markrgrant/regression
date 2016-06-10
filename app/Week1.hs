{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-- The purpose of this program is to perform linear regression on csv
-- data.  The input is:
-- 1. A csv file containing the training data
-- 2. A csv file containing the test data
-- 3. A list of the headers in the training data upon which the fitting is
--    to be performed, along with their types (string, int, float). 
-- 4. The header whose values is to be predicted.
--
-- The output is written to standard output and consists of the header from
-- 4, followed by a list of the predicted values, one for each row in the
-- test data.
--
-- The training data and test data are assumed to contain the same 
-- References on working with numbers in Haskell:
-- https://en.wikibooks.org/wiki/Haskell/Type_basics_II
--


import House
import Stat (slr, rss)
import FeatureChart

import Prelude hiding (id)
import Control.Applicative (pure)
import Control.Monad (mzero)
import System.IO (readFile)
import System.Environment (getArgs)
import Data.Csv hiding ((.=))
import GHC.Generics
import qualified Data.ByteString.Lazy as B
import qualified Data.Vector as V
import Data.Text (Text)
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Easy hiding (view)
import Graphics.Rendering.Chart.Grid
import Graphics.Rendering.Chart.Backend.Cairo (renderableToFile)



main :: IO ()
main = do
    [trainingFile, testFile] <- getArgs
    trainingData <- B.readFile trainingFile
    testData <- B.readFile testFile

    let trainingCSV = decodeByName trainingData :: Either String (Header, V.Vector House)
        testCSV = decodeByName testData :: Either String (Header, V.Vector House)
        output = price
                    {-
        features = [[bathrooms,sqft_living15],
                    [bedrooms, long,sqft_lot15],
                    [sqft_living, lat]]
                    -}
    case trainingCSV of
        Left err -> putStrLn err
        Right (trainingHeader, trainingHouses) -> 
            case testCSV of
                Left err -> putStrLn err
                Right (testHeader, testHouses) -> do
                    let charts = [[
                                featureChart "bathrooms" trainingHouses bathrooms price,
                                --customChart  "waterfront" trainingHouses waterfront price,
                                featureChart  "sqft above" trainingHouses (fromIntegral . sqft_above) price,
                                featureChart  "sqft living15" trainingHouses sqft_living15 price,
                                featureChart  "lat" trainingHouses lat price
                                ],
                                [
                                featureChart  "grade" trainingHouses (fromIntegral . grade) price,
                                featureChart  "yr renovated" trainingHouses (fromIntegral . yr_renovated) price, 
                                featureChart  "price" trainingHouses price price, 
                                featureChart  "bedrooms" trainingHouses bedrooms price
                                ],
                                [
                                --featureChart  "zipcode" trainingHouses zipcode price, 
                                featureChart  "long" trainingHouses long price, 
                                featureChart  "sqft lot15" trainingHouses sqft_lot15 price, 
                                featureChart  "sqft living" trainingHouses sqft_living price,
                                featureChart  "condition" trainingHouses (fromIntegral . condition) price
                                ],
                                --[
                                --featureChart  "floors" trainingHouses floors price
                                --featureChart  "date" trainingHouses date price
                                --],
                                [
                                featureChart  "sqft basement" trainingHouses (fromIntegral . sqft_basement) price,
                                featureChart  "year built" trainingHouses (fromIntegral . yr_built) price,
                                --id
                                featureChart  "sqft lot" trainingHouses (fromIntegral . sqft_lot) price,
                                featureChart  "view" trainingHouses (fromIntegral . view) price
                                ]
                               ]
                    putStrLn $ "training set size : " ++ show (V.length trainingHouses)
                    putStrLn $ "    test set size : " ++ show (V.length testHouses)
                    putStrLn $ "             sumx : " ++ show (V.sum (V.map sqft_living trainingHouses))
                    let (slope, intercept) = slr trainingHouses sqft_living price
                    let (slope2, intercept2) = slr trainingHouses bedrooms price
                    putStrLn $ "            slope : " ++ show slope
                    putStrLn $ "        intercept : " ++ show intercept
                    putStrLn $ "        2650 sqft : " ++ show (2650*slope + intercept)
                    putStrLn $ "              rss : " ++ show (rss trainingHouses sqft_living price slope intercept)
                    putStrLn $ "          $800000 : " ++ show ((800000-intercept)/slope)
                    putStrLn $ "  sqft_livingtest : " ++ show (rss testHouses sqft_living price slope intercept)
                    putStrLn $ "      bedroomtest : " ++ show (rss testHouses bedrooms price slope2 intercept2)
                    renderableToFile def "features.png" $ fillBackground def $ gridToRenderable $ featureGrid charts output "Housing Features"
                    return ()
    return ()
