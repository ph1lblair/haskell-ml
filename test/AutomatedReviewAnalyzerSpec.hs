module AutomatedReviewAnalyzerSpec where

import Numeric.LinearAlgebra as LA
import AutomatedReviewAnalyzer
import SentimentAnalysis
import Test.Hspec

theta :: Vector R
theta = LA.vector [1.0, 1.0] 

spec :: Spec
spec = do
  describe "Classify" $ do

    it "Classify 1" $ do
      let x = (3><2) [1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
          theta0 = 0
          m = classify x theta theta0
      m `shouldBe` LA.vector [1.0, 1.0, 1.0]

    it "Classify 2" $ do
      let x = (1><2) [-1.0, 1.0] 
          theta = LA.vector [1.0, 1.0] 
          theta0 = 0
          m = classify x theta theta0
      m `shouldBe` LA.vector [-1.0]

  describe "Classifier Accuracy 1" $ do

    it "Accuracy 1" $ do
      let x1 = (3><2) [1.0, 0, 1.0, -1.0, 2.0, 3.0]
          x1' = (2><2) [1.0, 1.0, 2.0, -1.0]
          y1 = LA.vector [1.0, -1.0, 1.0] 
          y1' = LA.vector [-1.0, 1.0]
          t1 = 1 
          m =  classifierAccuracy perceptron x1 x1' y1 y1' t1 [1, 2, 0]
      m `shouldBe` (1.0, 0.0)

    it "Accuracy 2" $ do
      let x2 = (3><2) [1.0, 0, 1.0, -1.0, 2.0, 3.0]
          x2' = (2><2) [1.0, 1.0, 2.0, -1.0]
          y2 = LA.vector [1.0, -1.0, 1.0] 
          y2' = LA.vector [-1.0, 1.0]
          t2 = 1
          lambda = 0.2
          m =  classifierAccuracy' pegasos x2 x2' y2 y2' t2 lambda [1, 2, 0]
      m `shouldBe` (1.0, 0.0)
 