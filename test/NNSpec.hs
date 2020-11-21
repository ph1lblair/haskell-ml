module NNSpec where

import Numeric.LinearAlgebra as LA
import Test.Hspec
import NN

spec :: Spec
spec =
  describe "Toy Neural Network" $

--    it "Train Point" $ do
--        let wT1 = (3 >< 2) $ repeat 1.0
--            b1 = (3 >< 1) $ repeat 0.0
--            vT1 = (1 >< 3) $ repeat 1.0
--            learningRate = 0.001
--            res = trainPoint wT1 b1 vT1 learningRate 2 1 10
--        res `shouldBe` (wT1, b1, vT1)
  
    it "Test" $ do
        let wT = (3 >< 2) $ repeat 1.0
            b = (3 >< 1) $ repeat 0.0
            vT = (1 >< 3) $ repeat 1.0
            learningRate = 0.001
            epochs = 10
            xyPairs = [((2.0, 1.0), 10.0), ((3.0, 3.0), 21.0), ((4.0, 5.0), 32.0), ((6.0, 6.0), 42.0)]
            (wT', b', vT') = trainNeuralNetwork xyPairs wT b vT learningRate epochs
            x = [(1.0,1.0), (2.0,2.0), (3.0,3.0), (5.0,5.0), (10.0,10.0)]
            expected = fmap (snd . fmap (* 7)) x
            res = testNeuralNetwork x wT' b' vT' learningRate
            res' = map ((< 0.1) . abs) $ zipWith (-) res expected
        res' `shouldBe` [True, True, True, True, True]
