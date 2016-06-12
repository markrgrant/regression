-- An abstract data type for performing multiple linear regression.

module Regression (
    model_features,
    model_outputs,
    model_weights,
    model_predictions,
    model_rss,
    model_iterations,
    feature_mean,
    create_model,
    create_features,
    predict
) where

import Data.Array.Repa as Repa hiding ((++))
import Data.Array.Repa.Algorithms.Matrix (mmultP)
import Data.List hiding (transpose)
import Data.Maybe (fromJust)
import Debug.Trace (trace)
import Stat (range)


-- A model is a feature matrix, a feature vector representing the observed
-- output, a weight vector containing the calculated vector of optimized
-- weights, and the predicted outputs
data Model = MO {
    model_features    :: FeatureMatrix,
    model_outputs     :: FeatureVector,
    model_weights     :: WeightVector,
    model_predictions :: FeatureVector,
    model_rss         :: Double,
    model_iterations  :: Int
} deriving (Show)


-- A two-dimensional matrix, whose columns represent named
-- features.
data FeatureMatrix = FM {
    fm_name_indexes:: [(String, Int)],
    fm_values :: (Array U DIM2 Double)
} deriving (Show)


-- A vector of Doubles, whose column represents a named feature.
data FeatureVector = FV {
    fv_name :: String,
    fv_values :: (Array U DIM2 Double)
}

instance Show FeatureVector where
    show (FV name values) =
        name ++ " = " ++ show (toList values)



-- A vector of weights.  Each weight corresponds to a named feature.
data WeightVector = WV {
    wv_name_indexes :: [(String, Int)],
    wv_values :: (Array U DIM2 Double)
}

instance Show WeightVector where
    show (WV name_indexes values) =
        concatMap showWeight (zip name_indexes (toList values))
        where showWeight ((name, i), value) = name ++ " = " ++ show value ++ "\n"



feature_mean :: FeatureMatrix -> String -> Double
feature_mean (FM name_indexes h) name =
    let i = fromJust $ Data.List.lookup name name_indexes
        column = slice h (Any :. (i::Int)) 
        (Z:.j) = extent column
        [total] = sumAllP column
    in total/(fromIntegral j)



-- Create a model from a list of records, a list of features, and the output
-- accessor function.  This computes the weight vector as well.
--
-- Add an initial feature as well, containing just 1's.  Make sure that the
-- normalization that is applied to other features is not applied to this
-- feature.
create_model :: [a] -> [(String, a -> Double)] -> (String, a -> Double) -> [Double] -> Double -> Double -> Model
create_model rows features (output_name, output) initial_weights e n = 
    let fmat = create_features rows features
        nn = length rows
        observations = FV output_name (fromListUnboxed (Z:.(nn::Int):.(1::Int)) (Data.List.map output rows))
        (weights,iters) = gradient_descent fmat observations (create_weights fmat initial_weights) e n
        predictions = predict fmat weights
        residuals = rss observations predictions
    in MO fmat observations weights predictions residuals iters


-- a helper method for multiplying a feature matrix by a weights vector to
-- obtain the predicted output
predict :: FeatureMatrix -> WeightVector -> FeatureVector
predict (FM _ h) (WV _ w) = 
    let [y'] = h `mmultP` w
    in FV "" y'


-- Generates the feature matrix, usually denoted as H, with N rows and
-- D features where D is the length of the feature list.
create_features :: [a] -> [(String, a -> Double)] -> FeatureMatrix
create_features inputs hs = 
    let n = length inputs
        d = length hs
        names = (Data.List.map fst hs)
        name_indexes = Prelude.zip names [0..]
        dat = [h(row) | row <- inputs, (_,h) <- hs]
        h = fromListUnboxed (Z:.n:.d) dat 
    in FM name_indexes h


-- Generates the feature matrix, usually denoted as H, with N rows and
-- D features where D is the length of the feature list.
--
-- An additional feature is added to the features representing the value
-- 1. 
scaled_feature_matrix :: [a] -> [(String, a -> Double)] -> FeatureMatrix
scaled_feature_matrix inputs hs = 
    let hs' = ("w0", const 1):hs
        n = length inputs
        d = length hs'
        names = (Data.List.map fst hs')
        --means = 0:(Data.List.map mean [[h(row) | row <- inputs] | (_, h) <- hs])
        --sds   = 1:(Data.List.map stdev [[h(row) | row <- inputs] | (_, h) <- hs])
        mins = 0:(Data.List.map minimum [[h(row) | row <- inputs] | (_, h) <- hs])
        r = 1:(Data.List.map range [[h(row) | row <- inputs] | (_, h) <- hs])
        name_indexes = Prelude.zip names [0..]
        hs_mean_range = Data.List.map (\((_,h),m,ra) -> (/ra) . (+(-m)) . h) (zip3 hs' mins r)
        dat = [hms(row) | row <- inputs, hms <- hs_mean_range]
        h = fromListUnboxed (Z:.n:.d) dat 
    in FM name_indexes h


-- Compute the residual sum of squares from the observed output and
-- the predicted output. 
rss :: (Num a) => FeatureVector -> FeatureVector -> Double
rss (FV _ v1) (FV _ v2) = rss' v1 v2


rss' :: Array U DIM2 Double -> Array U DIM2 Double -> Double
rss' v1 v2 =
    let p = Repa.zipWith sq v1 v2
        sq a b = (a - b)^2
        [result] = foldAllP (+) 0 p
    in result


{-
-- Get the weight of a particular feature in a weight vector.
weight_for_feature :: WeightVector -> String -> Maybe Double
weight_for_feature (WV name_indexes v) name = do
    index <- Prelude.lookup name name_indexes
    Just $ v ! (Z :. index :. 1)
-}

-- Get an empty vector of weights whose feature names are the same, and
-- in the same order, as the given feature matrix.
create_weights :: FeatureMatrix -> [Double] -> WeightVector
create_weights (FM name_indexes h) weights = 
    let (Z:.i:.j) = extent h
    in WV name_indexes $ computeS $ fromFunction (Z :. j :. 1) (\(Z:.j:.i) -> weights !! j :: Double)


-- perform gradient descent, updating the weight matrix until the residual
-- sum of squares is less than the epsilon value e, at which point the
-- weight matrix w is returned.  n is the step size.
gradient_descent :: FeatureMatrix -> FeatureVector -> WeightVector ->
    Double -> Double -> (WeightVector, Int)
gradient_descent (FM _ f) (FV _ o) (WV name w) e n =
    let [ft] = computeP $ transpose f
        (weights, count) = gradient_descent' f ft o w e n 0
    in (WV name weights, count)


gradient_descent' :: Array U DIM2 Double -> Array U DIM2 Double -> Array U DIM2 Double ->
    Array U DIM2 Double -> Double -> Double -> Int -> (Array U DIM2 Double, Int)
gradient_descent' h ht y w e n c =
    let grad = gradient h ht y w -- (-2H^t(y-Hw))
        grad_len = magnitude grad  -- grad RSS(w) == ||2H^t(y-HW)||
    in if (trace ("gradient size = " ++ show(grad_len) ++ ", tolerance = " ++ (show e)) grad_len) < e
    --in if grad_len < e
        then (w, c)
        else let delta = Repa.map (*(-n)) grad -- (2nH^t(y-Hw))
                 [w'] = computeP $ w +^ delta -- 
             in gradient_descent' h ht y w' e n (c+1)


-- calculate the gradient of the residual sum of squares (-2H^t(y-Hw)).
-- This is used to compute the magnitude of the gradient, to see if the 
-- function is minimized.  It is also used to update the weights of the
-- features.  A fra
gradient :: Array U DIM2 Double -> Array U DIM2 Double -> Array U DIM2 Double ->
    Array U DIM2 Double -> Array U DIM2 Double
gradient h ht y w =
    let [yhat] = h `mmultP` w
        [err] = computeP $ y -^ yhat
        [prod] = ht `mmultP` err
        [grad] = computeP $ Repa.map (*(-2)) prod
    in grad


-- compute the magnitude of the given vector
magnitude :: Array U DIM2 Double -> Double
magnitude vec = 
    let [total] = Repa.sumAllP $ Repa.map (\y -> y^2) vec
    in sqrt total
