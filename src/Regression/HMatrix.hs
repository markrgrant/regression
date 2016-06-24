-- An abstract data type for performing multiple linear regression, 
-- using HMatrix as the backend for matrix computations.
--
-- types:
--   Matrix a (2D-array)
--   Vector a (vector from the standard vector package)
--
-- Both types are dense, immutable, and strict in all elements and are 
-- manipulated as whole blocks.
--
-- matrix product is a <> b, where a and b are matrices
-- matrix-vector product is a #> x, where a is a matrix and x a vector
-- vector-vector dot product is x <.> y
-- the (conjugate) transpose is trans a, where a is a matrix or vector
-- the general linear system solver is <\>
--
-- to construct a 4 by 3 matrix: 
-- let a = (4><3) [1,2,3,4,5,6,7,8,9,10,11,12] :: Matrix R
--
-- to construct a vector of double-precision floating points:
-- let x = vector [1,2,3]
--
-- to multiply vectors:
-- x * y
--
-- to add two vectors elementwise: 
-- x + y
--
-- to compute vector dot product:
-- x <.> y
--
-- to perform matrix-vector product:
-- m  #> x
--
-- to perform matrix-matrix product
-- m <> n
--
-- to sum elements of a vector or matrix:
-- sumelements m
--

module Regression.HMatrix (
    model_features,
    model_outputs,
    model_weights,
    model_predictions,
    model_rss,
    create_model,
    create_features,
    create_weights,
    predict,
    powers,
    normalize,
    newtons_method,
    newtons_method_with_norm,
    cross_validate,
    gradient_descent
) where

import Numeric.LinearAlgebra hiding (magnitude)
import Numeric.LinearAlgebra.HMatrix hiding (magnitude)
import Data.List hiding (transpose)
import Data.Maybe (fromJust)
import Debug.Trace (trace)
import Stat (range, mean, stdev)
import qualified Data.Vector as V

-- A model is:
-- * a feature matrix with n rows and d columns
-- * a vector of length n representing the observed output
-- * a weight vector containing the calculated vector of optimized weights
-- * the predicted outputs using the optimized weights
-- * the 
data Model = MO {
    model_features    :: FeatureMatrix,
    model_outputs     :: FeatureVector,
    model_weights     :: WeightVector,
    model_predictions :: FeatureVector,
    model_rss         :: Double
} deriving (Show)


-- A two-dimensional matrix whose columns contain feature values. 
data FeatureMatrix = FM {
    fm_name_indexes:: [(String, Int)],
    fm_values :: Matrix Double
} deriving (Show)


-- A vector of values for a named feature.
data FeatureVector = FV {
    fv_name :: String,
    fv_values :: Vector Double
}
instance Show FeatureVector where
    show (FV name values) =
        name ++ " = " ++ show (toList values)


type Feature a = (String, a -> Double)

type Output a = (String, a -> Double)

type Optimizer = FeatureMatrix -> FeatureVector -> WeightVector


-- A vector of weights.  Each weight corresponds to a feature in the
-- feature matrix used to calculate the weights.
data WeightVector = WV {
    wv_name_indexes :: [(String, Int)],
    wv_values :: Vector Double
}


instance Show WeightVector where
    show (WV name_indexes values) =
        concatMap showWeight (zip name_indexes (toList values))
        where showWeight ((name, i), value) = name ++ " = " ++ show value ++ "\n"


-- Creates a model from a list of records, a list of features, and the output
-- accessor function.  This computes the weight vector as well.  This function
-- does not scale the features nor does it add an intercept feature (a feature
-- whose values are all 1). 
create_model :: [a] -> [Feature a] -> Output a -> Optimizer -> Model
create_model rows features (output_name, output) optimizer = 
    let fmat         = create_features features rows
        nn           = length rows
        observations = FV output_name (fromList (Data.List.map output rows))
        weights      = optimizer fmat observations 
        predictions  = predict fmat weights
        residuals    = rss observations predictions
    in MO fmat observations weights predictions residuals


-- Multiplies a feature matrix by a weights vector to obtain a prediction of
-- output.
predict :: FeatureMatrix -> WeightVector -> FeatureVector
predict (FM _ h) (WV _ w) = FV "" (h #> w)


-- Generates the feature matrix, usually denoted as H, with N rows and
-- D features where D is the length of the feature list.
create_features :: [Feature a] -> [a] -> FeatureMatrix
create_features hs inputs = 
    let n = length inputs
        d = length hs
        names = (Data.List.map fst hs)
        name_indexes = Prelude.zip names [0..]
        dat = [h(row) | row <- inputs, (_,h) <- hs]
        h = (n><d) dat 
    in FM name_indexes h


-- Computes the residual sum of squares from the observed output and
-- the predicted output. 
rss :: (Num a) => FeatureVector -> FeatureVector -> Double
rss (FV _ v1) (FV _ v2) = rss' v1 v2


rss' :: Vector Double -> Vector Double -> Double
rss' v1 v2 = diff <.> diff
    where diff = v1 - v2


-- Takes a list of numbers and turns them into a vector of weights.
create_weights :: FeatureMatrix -> [Double] -> WeightVector
create_weights (FM name_indexes h) weights = WV name_indexes (vector weights)


newtons_method :: Optimizer
newtons_method h y = newtons_method_with_norm 0 h y


newtons_method_with_norm :: Matrix Double -> Optimizer
newtons_method_with_norm l (FM n h) (FV o y) = WV n w
    where w = newtons_method_with_norm' l h y


-- Performs newtons method, finding an estimate of the minimum.  This method
-- uses the formula:
-- w_hat = (H^t * H - lambda*I)^-1 * H^T * y
newtons_method_with_norm' :: Matrix Double -> Matrix Double -> Vector Double -> Vector Double
newtons_method_with_norm' l h y = ((inv (hth - mod)) <> th) #> y
    where th = tr h
          hth = th <> h
          (d,_) = size hth
          mod = l * (ident d) :: Matrix Double



-- Performs gradient descent, updating the weights until the residual
-- sum of squares is less than the epsilon value e, at which point the
-- weight matrix is returned.  n is the step size.
gradient_descent :: Double -> Double -> WeightVector -> Optimizer
gradient_descent e n (WV name w) (FM _ f) (FV _ o) =
    let ft      = tr f
        weights = gradient_descent' e n w f ft o
    in WV name weights


gradient_descent' :: Double -> Double -> Vector Double -> Matrix Double ->
    Matrix Double -> Vector Double -> Vector Double
gradient_descent' e n w h ht y =
    let grad = gradient h ht y w -- (-2H^t(y-Hw))
        grad_len = magnitude grad  -- grad RSS(w) == ||2H^t(y-HW)||
    --in if grad_len < e
    in if (trace ("gradient = " ++ show grad_len) grad_len) < e
        then w
        else let delta = cmap (*(-n)) grad -- (2nH^t(y-Hw))
                 w' = w + delta
             in gradient_descent' e n w' h ht y


-- Compute the magnitude of the given vector.
magnitude :: Vector Double -> Double
magnitude vec = sumElements $ cmap (\y -> y^2) vec

-- Calculates the gradient of the residual sum of squares (-2H^t(y-Hw)).
-- This is used to compute the magnitude of the gradient, to see if the 
-- function is minimized.  It is also used to update the weights of the
-- features.
gradient :: Matrix Double -> Matrix Double -> Vector Double ->
    Vector Double -> Vector Double
gradient h ht y w =
    let yhat = h #> w
        err = y - yhat
        prod = ht #> err
        grad = cmap (*(-2)) prod
    in grad


-- Given a feature, computes polynomial powers of that feature from 
-- 1 (the original feature) up to and including n. 
powers :: Feature a -> Int -> [Feature a]
powers (name, f) n = Data.List.map  (\i -> (name ++ (show i), (\a -> (f a)^i))) [1..n]


-- given a list of values and a feature, return a normalized version of that
-- feature where the mean of the feature is subtracted from each feature and
-- the result is divided by the standard deviation of the feature.
normalize :: [a] -> (a -> Double) -> a -> Double
normalize xs f = (/sdxs) . (+(-meanxs)) . f
    where meanxs = mean (Data.List.map f xs)
          sdxs = stdev (Data.List.map f xs)


-- Compute the average k-fold cross-validation error for a given L2 penalty and
-- a given split size k.
-- Note that the rows should be shuffled before they are provided.
cross_validate :: Int -> [a] -> [Feature a] -> Output a  -> Optimizer -> Double
cross_validate k rows features o@(output_name, output) optimizer = kmean
    where ksplits         = ksplit k rows
          training_data   = map fst ksplits
          valid_data      = map snd ksplits
          trained_models  = map (\rows -> create_model rows features o optimizer) training_data
          trained_weights = map model_weights trained_models
          valid_features  = map (create_features features) valid_data 
          predictions     = zipWith (\h w -> predict h w) valid_features trained_weights
          observations    = map (\dat -> FV output_name (fromList (Data.List.map output dat))) valid_data
          valid_rss       = zipWith (\o p -> rss o p) observations predictions
          kmean           = mean valid_rss


-- prepare splits of the data for k-means where k is the number of
-- clusters and the number of values in the test set (the second list
-- in the pair) is n/k where n is the size of the input list.
ksplit :: Int -> [a] -> [([a], [a])]
ksplit k xs = map (ksplit' k xs) [0..k-1]


-- prepare a k split ([a], [a]) for the chunk at index i with list xs
-- and with number of chunks k
ksplit' :: Int -> [a] -> Int -> ([a], [a])
ksplit' k xs i = ((take (sz*i) xs) ++ (drop (sz*(i+1)) xs), take sz (drop (sz*i) xs))
    where sz = length xs `div` k
