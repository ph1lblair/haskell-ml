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

computeProbabilities :: Matrix R -> Matrix R -> R -> Matrix R
computeProbabilities x theta temp = p / divisor
    where
        c = (rows theta >< rows x) $ concat $ repeat $ map (maximum . toList) $ toColumns $ (theta LA.<> tr x) / scalar temp
        p = cmap exp $ (theta LA.<> tr x) / scalar temp - c
        divisor = (rows theta >< rows x) $ concat $ repeat $ map (sum . toList) $ toColumns p

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



