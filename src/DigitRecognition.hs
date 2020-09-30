{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
module DigitRecognition where

import Numeric.LinearAlgebra as LA
import Codec.Compression.GZip (decompress)
import qualified Data.ByteString.Lazy as BS
import Data.Functor
import qualified Foreign.Storable as ForeignStorable
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

clip a b = max a . min b

computeProbabilities :: Matrix R -> Matrix R -> R -> Matrix R
computeProbabilities x theta temp = cmap (\x -> if x < 1.0e-16 then 0 else x) p / divisor
    where
        c = (rows theta >< rows x) $ concat $ repeat $ map (maximum . toList) $ toColumns $ (theta LA.<> tr x) / scalar temp
        p = cmap exp $ (theta LA.<> tr x) / scalar temp - c
        divisor = (rows theta >< rows x) $ concat $ repeat $ map (sum . toList) $ toColumns p

computeCostFunction :: Matrix R -> Vector R -> Matrix R -> R -> R -> R
computeCostFunction x y theta lambda temp = 3.0
    where
        n = rows x
        d = cols x
        k = rows theta
        -- contributors = (n >< k) $ repeat 0.0
--        X - (n, d) NumPy array (n datapoints each with d features)
--        Y - (n, ) NumPy array containing the labels (a number from 0-9) for each
--        theta - (k, d) NumPy array, where row j represents the parameters of our
--                model for label j
--    for i in range(n):
--        for j in range(k):
--            if Y[i] == j:
--                contributors[i, j] = 1
--    log_probabilities = np.log(np.clip(compute_probabilities(X, theta, temp_parameter), 1e-15, 1-1e-15))
--    loss = -1/n * np.sum(np.transpose(contributors) * log_probabilities)
--    regularisation = 1/2 * lambda_factor * np.sum(np.square(theta))
--    return loss + regularisation

buildData :: (ForeignStorable.Storable Float, Num Float) => BS.ByteString -> Matrix Float
buildData ds = (60000 >< (28 ^ 2)) $ map fromIntegral $ BS.unpack $ BS.drop 16 ds

buildLabels :: (ForeignStorable.Storable Int, Num Int) => BS.ByteString -> Vector Int
buildLabels bs = fromList $ map fromIntegral $ BS.unpack $ BS.drop 8 bs

main :: IO ()
main = do
    t <- decompress <$> BS.readFile "train-images-idx3-ubyte.gz"
    l <- decompress <$> BS.readFile "train-labels-idx1-ubyte.gz"

    let trainData = buildData t
    let trainLabels = buildLabels l
    
    print $ take 5 $ toList trainLabels



