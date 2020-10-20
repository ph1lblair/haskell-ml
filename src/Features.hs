{-# LANGUAGE FlexibleContexts #-}
module Features where

import Numeric.LinearAlgebra as LA
import Numeric.LinearAlgebra.HMatrix (eigSH')
import Codec.Compression.GZip (decompress)
import qualified Data.ByteString.Lazy as BS
import DigitRecognition
import Data.Bifunctor (Bifunctor(bimap))

mean :: Matrix R -> Vector R
mean x = tr x #> konst k r
    where
        r = rows x
        k = 1 / fromIntegral r        

centreData :: Matrix R -> Matrix R
centreData x = x - fromRows (replicate (rows x) (mean x))

principalComponents :: Matrix R -> Matrix R
principalComponents x = vs
    where
        x' = centreData x
        cov = tr x' LA.<> x'
        (_, vs) = eigSH' cov

projectOntoPC :: Matrix R -> Matrix R -> Int -> Matrix R
projectOntoPC x pcs n = cmap (\x -> if x < 1.0e-16 && x > -1.0e-16 then 0 else x) $ centreData x LA.<> (pcs ¿ [0 .. n - 1])

triUpper :: Int -> Int -> [(Int, Int)]
triUpper m offset = [(a,b) | a <- [0.. m - 1], b <- [offset .. m - 1], a + offset <= b]

twoTerms :: [R] -> [R]
twoTerms xs = concat $ twoTerms' (drop 1 xs) xs
    where
        twoTerms' [] _ = []
        twoTerms' xs ys = twoTerms'' (zip xs ys) ++ twoTerms' (drop 1 xs) ys
        twoTerms'' xs = map (\(x, y) -> [sqrt 6 * x * y]) xs 

threeTerms :: Int -> Int -> Int -> [(Int, Int)]
threeTerms m i offset = threeTerms' m i offset
    where
        threeTerms' 1 _ _ = []
        threeTerms' m i offset = fmap (bimap (offset +) (+i)) (triUpper m 1) ++ threeTerms' (m - 1) (i+1) (offset + m)

squareTerms :: [R] -> [R]
squareTerms xs = concat $ squareTerms' (drop 1 xs) xs  
    where
        squareTerms' [] _ = []
        squareTerms' xs ys = squareTerms'' (zip xs ys) ++ squareTerms' (drop 1 xs) ys
        squareTerms'' xs = map (\(x, y) -> [sqrt 3 * x ** 2 * y, sqrt 3 * x * y ** 2]) xs

crossTerms :: Vector R -> [R]
crossTerms v = coords
    where
        symm = v `outer` v
        pairs = vjoin $ zipWith pairs' (toRows symm) [1..]
        triples = pairs `outer` v
        pairs' vs i = subVector i (size vs - i) vs
        coords = map ((sqrt 6 * ) . (triples `atIndex`)) $ threeTerms (cols triples - 1) 1 0

cubicFeatures :: Matrix R -> Matrix R
cubicFeatures x = cubeTerms ||| (n >< squareCols) squareTerms' ||| (n >< linearCols) linearTerms' ||| (n >< crossCols) crossTerms'
    where
        n = rows x
        x' = x ||| (rows x >< 1) (replicate n 1.0)
        cubeTerms = cmap (**3) x'
        squareTerms' = concatMap (squareTerms . toList) (toRows x')
        linearTerms' = concatMap (twoTerms . toList) (toRows x)
        crossTerms' = concatMap crossTerms (toRows x)
        squareCols = length squareTerms' `div` n
        linearCols = length linearTerms' `div` n
        crossCols = length crossTerms' `div` n
        

main :: IO ()
main = do
    
    tr <- decompress <$> BS.readFile "train-images-idx3-ubyte.gz"
    trl <- decompress <$> BS.readFile "train-labels-idx1-ubyte.gz"
    te <- decompress <$> BS.readFile "t10k-images-idx3-ubyte.gz"
    tel <- decompress <$> BS.readFile "t10k-labels-idx1-ubyte.gz"

    let trainData = buildData 60000 tr
    let trainLabels = buildLabels trl
    let testData = buildData 10000 te
    let testLabels = buildLabels tel
    let temp = 1
    let alpha = 0.3
    let lambda = 0.0001
    let k = 10
    let iter = 150

--    let nComponents = 18
    let pcs = principalComponents trainData
--    let trainData' = projectOntoPC trainData pcs nComponents
--    let testData' = projectOntoPC testData pcs nComponents

--    let (theta, cs) = softmaxRegression trainData' trainLabels temp alpha lambda k iter
--    print $ computeTestError testData' testLabels theta temp

    let nCubic = 10
    let trainData'' = projectOntoPC trainData pcs nCubic
    let testData'' = projectOntoPC testData pcs nCubic

    let trainCube = cubicFeatures trainData''
    let testCube = cubicFeatures testData''

    let (theta', _) = softmaxRegression trainCube trainLabels temp alpha lambda k iter
    print $ computeTestError testCube testLabels theta' temp
