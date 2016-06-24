{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

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
