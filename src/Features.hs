module Features where

import Numeric.LinearAlgebra as LA
import Numeric.LinearAlgebra.HMatrix (eigSH')
import Codec.Compression.GZip (decompress)
import qualified Data.ByteString.Lazy as BS
import DigitRecognition

mean :: Matrix R -> Vector R
mean x = tr x #> konst k r
    where
        r = rows x
        k    = 1 / fromIntegral r        

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

linearTerms :: [R] -> [R]
linearTerms xs = concat $ linearTerms' (drop 1 xs) xs
    where
        linearTerms' [] _ = []
        linearTerms' xs ys = linearTerms'' (zip xs ys) ++ linearTerms' (drop 1 xs) ys
        linearTerms'' xs = map (\(x, y) -> [sqrt 6 * x * y]) xs

squareTerms :: [R] -> [R]
squareTerms xs = concat $ squareTerms' (drop 1 xs) xs  
    where
        squareTerms' [] _ = []
        squareTerms' xs ys = squareTerms'' (zip xs ys) ++ squareTerms' (drop 1 xs) ys
        squareTerms'' xs = map (\(x, y) -> [sqrt 3 * x ** 2 * y, sqrt 3 * x * y ** 2]) xs

cubicFeatures :: Matrix R -> Matrix R
cubicFeatures x = cubeTerms ||| squareTerms' ||| linearTerms'
    where
        n = rows x
        d = cols x
        x' = x ||| (rows x >< 1) (replicate (rows x) 1.0)
        cubeTerms = cmap (**3) x'
        squareTerms' = (n >< product [1.. n + 1]) $ concatMap (squareTerms . toList) (toRows x')
        linearTerms' = (n >< 1) $ concatMap (linearTerms . toList) (toRows x)

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

    let nComponents = 18
    let pcs = principalComponents trainData
    let trainData' = projectOntoPC trainData pcs nComponents
    let testData' = projectOntoPC testData pcs nComponents

    let (theta, cs) = softmaxRegression trainData' trainLabels temp alpha lambda k iter
    print $ computeTestError testData' testLabels theta temp

    let nCubic = 10
    let trainData'' = projectOntoPC trainData pcs nCubic
    let testData'' = projectOntoPC testData pcs nCubic

    let trainCube = cubicFeatures trainData''
    let testCube = cubicFeatures testData''

    let (theta', _) = softmaxRegression trainCube trainLabels temp alpha lambda k iter
    print $ computeTestError testCube testLabels theta temp
