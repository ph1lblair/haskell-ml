module DigitRecognitionSpec where

import Test.Hspec
import DigitRecognition
import Numeric.LinearAlgebra as LA

n :: Int
n = 3
d :: Int
d = 5 
k :: Int
k = 7
x :: Matrix R
x = (n >< d) $ map fromIntegral [0..(n * d - 1)]
zeros :: Matrix R
zeros = (k >< d) $ repeat 0.0
temp :: R
temp = 0.2

spec :: Spec
spec =
  describe "Digit Recognition" $ do

    it "Compute probabilities 1" $ do
        let
            zeros = (k >< d) $ repeat 0
            expected = (k >< n) $ repeat (1.0 / fromIntegral k)
            result1 = computeProbabilities x zeros temp
        result1 `shouldBe` expected

    it "Compute probabilities 2" $ do
        let
            theta = (n >< d) $ map fromIntegral [0..(n * d - 1)]
            temp = 0.2
            expected = (k >< (n - 1)) (repeat 0.0) ||| konst 1.0 (k, 1)
            result2 = computeProbabilities x theta temp
        result2 `shouldBe` expected
