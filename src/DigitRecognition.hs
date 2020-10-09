{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
module DigitRecognition where

import Numeric.LinearAlgebra as LA
import Codec.Compression.GZip (decompress)
import qualified Data.ByteString.Lazy as BS
import Data.Functor
import qualified Foreign.Storable as ForeignStorable
import GHC.Float (float2Double)
--import System.Random

render :: Integral a => a -> Char
render n = let s = " .:oO@" in s !! (fromIntegral n * length s `div` 256)

--main :: IO ()
--main = do
--  s <- decompress <$> BS.readFile "train-images-idx3-ubyte.gz"
--  l <- decompress <$> BS.readFile "train-labels-idx1-ubyte.gz"
--  n <- (`mod` 60000) <$> randomIO
--  putStr . unlines $
--    [render . BS.index s . (n * 28 ^ 2 + 16 + r * 28 +) <$> [0 .. 27] | r <- [0..27]]
-- print $ BS.index l (n + 8)

clip :: Ord c => c -> c -> c -> c
clip a b = max a . min b

computeProbabilities :: Matrix R -> Matrix R -> R -> Matrix R
computeProbabilities x theta temp = cmap (\x -> if x < 1.0e-16 then 0 else x) p / divisor
    where
        c = (rows theta >< rows x) $ concat $ repeat $ map (maximum . toList) $ toColumns $ (theta LA.<> tr x) / scalar temp
        p = cmap exp $ (theta LA.<> tr x) / scalar temp - c
        divisor = (rows theta >< rows x) $ concat $ repeat $ map (sum . toList) $ toColumns p

contributors :: (ForeignStorable.Storable R, Fractional R) => Int -> Int -> Vector Int -> Matrix R
contributors n k y = (n >< k) $ contributor k y 
    where
        contributor k = concatMap (contributor' k) . toList
        contributor' k v = l ++ [1.0] ++ r
            where
                (l, r) = splitAt v $ replicate (k - 1) 0.0

computeCostFunction :: Matrix R -> Vector Int -> Matrix R -> R -> R -> R
computeCostFunction x y theta lambda temp = loss (rows x) (rows theta) y + regularisation
    where
        logProbabilities = cmap log $ cmap (clip 1e-15 (1 - 1e-15)) (computeProbabilities x theta temp)
        regularisation = lambda / 2 * sumElements (cmap (** 2) theta)
        loss n k y' = -sumElements (tr (contributors n k y') * logProbabilities) / fromIntegral n

runGradientDescentIteration :: Matrix R -> Vector Int -> Matrix R -> R -> R -> R -> Matrix R
runGradientDescentIteration x y theta alpha lambda temp = theta - scale alpha grad
    where
        n = rows x
        k = rows theta
        m = tr $ contributors n k y
        probs = cmap (clip 1e-15 (1 - 1e-15)) (computeProbabilities x theta temp)
        grad = scale lambda theta - (m - probs) LA.<> x / (scalar temp * fromIntegral n)

augmentFeatures :: (Element R, Fractional R) => Matrix R -> Matrix R
augmentFeatures x = (rows x >< 1) (repeat 1.0) ||| x

softmaxRegression :: Matrix R -> Vector Int -> R -> R -> R -> Int -> Int -> (Matrix R, [R])
softmaxRegression x y temp alpha lambda k t = iterate step ((k >< cols x') $ repeat 0.0, []) !! t
    where
        x' = augmentFeatures x
        step (theta, cs) =
            (runGradientDescentIteration x' y theta alpha lambda temp, cs ++ [computeCostFunction x' y theta lambda temp])

getClassifification :: Matrix R -> Matrix R -> R -> [Int]
getClassifification x theta temp = map maxIndex $ toColumns probs
    where
        x' = augmentFeatures x
        probs = computeProbabilities x' theta temp

computeTestError :: Matrix R -> Vector Int -> Matrix R -> R -> R
computeTestError x y theta temp = 1 - sum (compare y assignedLabels) / fromIntegral (length assignedLabels)
    where
        assignedLabels = getClassifification x theta temp
        compare y z = zipWith (\x y -> if x == y then 1 else 0) (toList y) z

buildData :: (ForeignStorable.Storable Float, Num Float) => Int -> BS.ByteString -> Matrix R
buildData length ds = cmap float2Double $ (length >< (28 ^ 2)) $ map ((/ 256) . fromIntegral) $ BS.unpack $ BS.drop 16 ds

buildLabels :: (ForeignStorable.Storable Int, Num Int) => BS.ByteString -> Vector Int
buildLabels bs = fromList $ map fromIntegral $ BS.unpack $ BS.drop 8 bs

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
    let temp = 0.5
    let alpha = 0.3
    let lambda = 0.0001
    let k = 10
    let iter = 150
    let (theta, cs) = softmaxRegression trainData trainLabels temp alpha lambda k iter
--    print $ trainData ? [0]
--    print $ toList trainLabels !! 0
--    print $ (computeProbabilities' (augmentFeatures trainData) theta temp) ¿ [0]
--    let theta' = runGradientDescentIteration (augmentFeatures trainData) trainLabels theta alpha lambda temp
--    print $ computeCostFunction (augmentFeatures trainData) trainLabels theta lambda temp
--    print $ theta ¿ [0]
--    print $ getClassifification testData theta temp
    print $ computeTestError testData testLabels theta temp
    -- print $ take 5 $ toList trainLabels



