-- Various stat functions.   This library has no dependencies.
-- It may be best to switch to operating on Repa arrays.
module Stat where



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
    in sum (map (\x -> (x - m)^2) lst)



