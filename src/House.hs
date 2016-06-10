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
module House where

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


data House = House {
    bathrooms     :: !Double,
    waterfront    :: !Waterfront,
    sqft_above    :: !Int,
    sqft_living15 :: !Double,
    grade         :: !Int,
    yr_renovated  :: !Int,
    price         :: !Double,
    bedrooms      :: !Double,
    zipcode       :: !Text,
    long          :: !Double,
    sqft_lot15    :: !Double,
    sqft_living   :: !Double,
    floors        :: !Text,
    condition     :: !Int,
    lat           :: !Double,
    date          :: !Text,
    sqft_basement :: !Int,
    yr_built      :: !Int,
    id            :: !Text,
    sqft_lot      :: !Int,
    view          :: !Int
} deriving Generic

instance FromNamedRecord House
instance ToNamedRecord House
instance DefaultOrdered House


data Waterfront = Waterfront | NotWaterfront deriving (Eq, Ord)


instance FromField Waterfront where
    parseField s
        | s == "0" = pure NotWaterfront
        | s == "1" = pure Waterfront
        | otherwise = mzero


instance ToField Waterfront where
    toField Waterfront = "1"
    toField NotWaterfront = "0"


instance PlotValue Waterfront where
    toValue Waterfront = 1
    toValue NotWaterfront = 0
    fromValue 1 = Waterfront
    fromValue 0 = NotWaterfront
    autoAxis = \dat -> makeAxis labelingFn (dat, dat, dat)
        where labelingFn Waterfront = "Waterfront"
              labelingFn NotWaterfront = "Not Waterfront"
