-- Various stat functions.   This library has no dependencies.
-- It may be best to switch to operating on Repa arrays.
module Stat where


import Data.Set


mean :: [Double] -> Double
mean lst = (sum lst) / (fromIntegral (length lst))


range :: [Double] -> Double
range lst = 
    let x1 = minimum lst
        x2 = maximum lst
    in x2 - x1


-- sample standard deviation
stdev :: [Double] -> Double
stdev lst = sqrt (var lst)


var :: [Double] -> Double
var lst = 
    let m = mean lst
    in sum (Prelude.map (\x -> (x - m)^2) lst)


-- N(B)
freq :: Ord a => Set a -> [a] -> Int
freq b xs = length $ Prelude.filter (\x -> member x b) xs


-- ^P(B)
relfreq :: (Ord a, Fractional b) => Set a -> [a] -> b
relfreq b xs = fromIntegral (freq b xs) / fromIntegral (length xs)

