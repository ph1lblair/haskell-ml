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
y :: Vector Int
y = fromList [0..(n - 1)]
zeros :: Matrix R
zeros = (k >< d) $ repeat 0.0
temp :: R
temp = 0.2
lambda :: R
lambda = 0.5

spec :: Spec
spec = do
    describe "Compute probabilities" $ do

        it "Compute probabilities 1" $ do
            let
                expected = (k >< n) $ repeat (1.0 / fromIntegral k)
                result1 = computeProbabilities x zeros temp
            result1 `shouldBe` expected

        it "Compute probabilities 2" $ do
            let
                theta = (k >< d) $ map fromIntegral [0..(k * d - 1)]
                temp = 0.2
                expected = ((k - 1) >< n) (repeat 0.0) === konst 1.0 (1, n)
                result2 = computeProbabilities x theta temp
            result2 `shouldBe` expected

    describe "Compute cost" $

        it "Compute cost 1" $ do
            let
                cost1 = computeCostFunction x y zeros lambda temp
            cost1 `shouldBe` 1.9459101490553135

    describe "Gradient descent" $ 

        it "Run gradient descent" $ do
            let
                alpha = 2
                expected = (k >< d) [ -7.142857142857142, -5.238095238095236, -3.333333333333332, -1.4285714285714273, 0.4761904761904778
                  ,  9.523809523809526, 11.428571428571429, 13.333333333333332, 15.238095238095239, 17.142857142857142
                  ,  26.19047619047619, 28.095238095238095, 29.999999999999996, 31.90476190476191, 33.80952380952381
                  , -7.142857142857142,  -8.57142857142857, -10.0, -11.428571428571427, -12.857142857142854
                  , -7.142857142857141,  -8.57142857142857, -10.0, -11.428571428571427, -12.857142857142854
                  , -7.142857142857141,  -8.57142857142857, -10.0, -11.428571428571427, -12.857142857142854
                  , -7.142857142857141,  -8.57142857142857, -10.0, -11.428571428571427, -12.857142857142854 ]
                m = runGradientDescentIteration x y zeros alpha lambda temp
            m `shouldBe` expected