module AutomatedReviewAnalyzer where

import Numeric.LinearAlgebra as LA

classify :: Matrix R -> Vector R -> R -> Vector R
classify x theta thata0 = cmap check (theta LA.<# LA.tr' x + theta0s)
    where
        theta0s = rows x LA.|> repeat 0
        check z = if z > 0 then 1.0 else -1.0 

accuracy :: Vector R -> Vector R -> R
accuracy pred target = sum xs / fromIntegral (length xs)
    where
        xs = zipWith check (toList pred) (toList target)
        check a b = if a == b then 1 else 0

classifierAccuracy :: (Matrix R -> Vector R -> Int -> [Int] -> (Vector R, R))
                            -> Matrix R -> Matrix R -> Vector R -> Vector R -> Int -> [Int] -> (R, R)
classifierAccuracy classifier x x' y y' t zs = 
    (accuracy (classify x theta theta0) y, accuracy (classify x' theta theta0) y')
    where
        (theta, theta0) = classifier x y t zs
 
classifierAccuracy' :: (Matrix R -> Vector R -> Int -> R -> [Int] -> (Vector R, R))
                            -> Matrix R -> Matrix R -> Vector R -> Vector R -> Int -> R -> [Int] -> (R, R)
classifierAccuracy' classifier x x' y y' t lambda zs = 
    (accuracy (classify x theta theta0) y, accuracy (classify x' theta theta0) y')
    where
        (theta, theta0) = classifier x y t lambda zs
 