-- An abstract data type for performing multiple linear regression.

module Regression where

import Data.Array.Repa as Repa hiding ((++))
import Data.Array.Repa.Algorithms.Matrix (mmultP)
import Data.List hiding (transpose)
import Data.Maybe (fromJust)
import Debug.Trace (trace)


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
    fm_name_indexes:: [(String, (Int, Double, Double))],
    fm_values :: (Array U DIM2 Double)
} deriving (Show)


-- A vector of Doubles, whose column represents a named feature.
data FeatureVector = FV {
    fv_name :: String,
    fv_values :: (Array U DIM2 Double)
} deriving (Show)


-- A vector of weights.  Each weight corresponds to a named feature.
data WeightVector = WV {
    wv_name_indexes :: [(String, (Int, Double, Double))],
    wv_values :: (Array U DIM2 Double)
} deriving (Show)


print_weights :: WeightVector -> String
print_weights (WV name_indexes values) =
        concatMap showWeight (zip name_indexes (toList values))
        where showWeight ((name, (i, mean, sd)), value) = name ++ " = " ++ show (sd * value + mean) ++ "\n"


-- Create a model from a list of records, a list of features, and the output
-- accessor function.  This computes the weight vector as well.
create :: [a] -> [(String, a -> Double)] -> (String, a -> Double) -> Model
create rows features (name, output) = 
    let fmat = feature_matrix rows features
        nn = length rows
        observations = FV name (fromListUnboxed (Z:.(nn::Int):.(1::Int)) (Data.List.map output rows))
        e = 0.01*(fromIntegral nn) -- convergence criterion
        n = 0.001 -- step size
        (weights,iters) = gradient_descent fmat observations (empty_weight fmat) e n
        predictions = predict fmat weights
        residuals = rss observations predictions
    in MO fmat observations weights predictions residuals iters


-- a helper method for multiplying a feature matrix by a weights vector to
-- obtain the predicted output
predict :: FeatureMatrix -> WeightVector -> FeatureVector
predict (FM _ h) (WV _ w) = 
    let [y'] = h `mmultP` w
    in FV "" y'


-- Retrieve the feature names used to construct the feature matrix.
feature_names :: FeatureMatrix -> [String]
feature_names (FM name_indexes _) = Prelude.map fst name_indexes


-- Generates the feature matrix, usually denoted as H, with N rows and
-- D features where D is the length of the feature list.
--
-- An additional feature is added to the features representing the value
-- 1. 
feature_matrix :: [a] -> [(String, a -> Double)] -> FeatureMatrix
feature_matrix inputs hs = 
    let n = length inputs
        d = length hs
        names = "ones":(Data.List.map fst hs)
        means = (Data.List.map mean [[h(row) | row <- inputs] | (_, h) <- hs])
        sds   = (Data.List.map stdev [[h(row) | row <- inputs] | (_, h) <- hs])
        name_indexes = Prelude.zip names (Prelude.zip3 [0..] means sds)
        wut = trace ("name_indexes = " ++ show name_indexes) ()
        hs_mean_sd = Data.List.map (\((_,h),m,sd) -> (/sd) . (+(-m)) . h) (zip3 hs means sds)
        --hs_mean_sd = Data.List.map (\((_,h),m,sd) -> h) (zip3 hs means sds)
        dat = [hms(row) | row <- inputs, hms <- hs_mean_sd]
        h = fromListUnboxed (Z:.n:.d) dat 
    in FM name_indexes h


mean :: [Double] -> Double
mean lst = (sum lst) / (fromIntegral (length lst))


-- sample standard deviation
stdev :: [Double] -> Double
stdev lst = sqrt (var lst)


var :: [Double] -> Double
var lst = 
    let m = mean lst
    in sum (Data.List.map (\x -> (x - m)^2) lst)


feature_mean :: FeatureMatrix -> String -> Double
feature_mean (FM name_indexes h) name =
    let (i,mean,sd) = fromJust $ Data.List.lookup name name_indexes
    in mean

{-
traverse :: (Shape sh', Shape sh, Source r a)
     => Array r sh a            -- Source array
     -> (sh -> sh')             -- Function to produce the extent of the result.
     -> ((sh -> a) -> sh' -> b) -- Function to produce elements of the result.
                                -- It is passed a lookup function to
                                -- get elements of the source.
     -> Array D sh' b
-}

-- Return the feature with the given name in the feature vector.
lookup :: FeatureMatrix -> String -> FeatureVector
lookup matrix string = undefined


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


-- Get the weight of a particular feature in a weight vector.
weight :: WeightVector -> String -> Maybe Double
weight (WV name_indexes v) name = do
    (index, _, _) <- Prelude.lookup name name_indexes
    Just $ v ! (Z :. index :. 1)


-- Get an empty vector of weights whose feature names are the same, and
-- in the same order, as the given feature matrix.
empty_weight :: FeatureMatrix -> WeightVector
empty_weight (FM name_indexes h) = 
    let (Z:.i:.j) = extent h
    in WV name_indexes $ computeS $ fromFunction (Z :. j :. 1) (\(Z:.j:.i) -> 0 :: Double)


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
    in if (trace ("gradient size = " ++ show(grad_len)) grad_len) < e
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
