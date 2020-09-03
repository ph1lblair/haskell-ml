{-# LANGUAGE FlexibleContexts #-}
module SentimentAnalysis where

import Numeric.LinearAlgebra as LA
import Data.Bifunctor (Bifunctor(bimap))

hingeLossSingle :: (Ord a, Numeric a) => Vector a -> a -> Vector a -> a -> a
hingeLossSingle x y theta theta0 = max 0 (1 - y * (theta LA.<.> x + theta0))

hingeLossFull :: (Numeric a, Ord a, Num (Vector a), Fractional a) => Matrix a -> Vector a -> Vector a -> a -> a
hingeLossFull x y theta theta0 = loss
    where
        theta0s = size y |> repeat theta0
        ones = size y |> repeat 1.0
        agreement = y * (theta LA.<# LA.tr' x + theta0s)
        losses = LA.cmap (max 0) (ones - agreement)
        loss = sumElements losses / fromIntegral (size losses)

perceptronSingleStepUpdate :: (Ord b, Numeric b, Num (Vector b)) => Vector b -> b -> Vector b -> b -> (Vector b, b)
perceptronSingleStepUpdate x y theta theta0 = 
    if y * (theta LA.<.> x + theta0) <= 0
        then (theta + (scalar y * x), theta0 + y)
        else (theta, theta0)

perceptronUpdate :: (Ord t, Numeric t, Num (Vector t)) => [Vector t] -> [t] -> Vector t -> t -> (Vector t, t)
perceptronUpdate [] [] theta theta0 = (theta, theta0)
perceptronUpdate (x':xs) (y':ys) theta theta0 = 
    let (theta', theta0') = perceptronSingleStepUpdate x' y' theta theta0
    in perceptronUpdate xs ys theta' theta0'
    
perceptron :: (Ord t, Numeric t, Num (Vector t)) => Matrix t -> Vector t -> Int -> (Vector t, t)
perceptron x y t = iterate step (cols x |> repeat 0, 0) !! t
    where step (theta, theta0) = perceptronUpdate (toRows x) (toList y) theta theta0

averagePerceptronUpdate :: (Ord t, Numeric t, Num (Vector t)) => [Vector t] -> [t] -> Vector t -> t -> [(Vector t, t)]
averagePerceptronUpdate [] [] theta theta0 = []
averagePerceptronUpdate (x':xs) (y':ys) theta theta0 = 
    let (theta', theta0') = perceptronSingleStepUpdate x' y' theta theta0
    in (theta', theta0') : averagePerceptronUpdate xs ys theta' theta0'

averagePerceptronList :: (Ord t, Numeric t, Num (Vector t)) => Matrix t -> Vector t -> [[(Vector t, t)]]
averagePerceptronList x y = iterate step [(cols x |> repeat 0, 0)]
    where
        step xs = averagePerceptronUpdate (toRows x) (toList y) theta theta0
            where (theta, theta0) = last xs

averagePerceptron :: (Fractional t, Ord t, Numeric t, Num (Vector t)) => Matrix t -> Vector t -> Int -> (Vector t, t)
averagePerceptron x y t = (bimap ((/ scalar n) . sum) ((/ n) . sum) . unzip . concat) xss
    where 
        n = fromIntegral $ t * rows x
        xss = drop 1 $ take (t + 1) $ averagePerceptronList x y 

pegasosSingleStepUpdate :: (Ord e, Numeric e, Num (Vector e)) => Vector e -> e -> e -> e -> Vector e -> e -> (Vector e, e)
pegasosSingleStepUpdate x y lambda eta theta theta0 = 
    if y * (theta LA.<.> x + theta0) <= 1
        then
            (scalar (1 - eta * lambda) * theta + scalar (eta * y) * x, theta0 + eta * y)
        else 
            (scalar (1 - eta * lambda) * theta, theta0)

pegasosUpdate :: (Ord t, Numeric t, Num (Vector t), Floating t) => [Vector t] -> [t] -> t -> t -> Vector t -> t -> (Vector t, t, t)
pegasosUpdate [] [] lambda i theta theta0 = (theta, theta0, i)
pegasosUpdate (x':xs) (y':ys) lambda i theta theta0 = 
    let (theta', theta0') = pegasosSingleStepUpdate x' y' lambda (1 / sqrt i) theta theta0  
    in pegasosUpdate xs ys lambda (i + 1) theta' theta0'
    
pegasos :: (Ord t, Numeric t, Floating t, Num (Vector t)) => Matrix t -> Vector t -> Int -> t -> (Vector t, t)
pegasos x y t lambda = (theta', theta0')
    where
        (theta', theta0', i') = iterate step (cols x |> repeat 0, 0, 1.0) !! t
        step (theta, theta0, i) = pegasosUpdate (toRows x) (toList y) lambda i theta theta0

main :: IO ()
main = do
    dta <- loadMatrix "toy_data.tsv"
    
    let t = 10
    let lambda = 0.2
    let x = dta ¿ [1, 2]
    let y = flatten $ dta ¿ [0]

    let (theta, theta0) = perceptron x y t
    putStrLn "Perceptron:"
    print $ theta
    print $ theta0
