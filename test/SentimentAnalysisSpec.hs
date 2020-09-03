module SentimentAnalysisSpec where

import Numeric.LinearAlgebra as LA
import SentimentAnalysis
import Test.Hspec

theta :: Vector R
theta = LA.vector [-1.0, 1.0]

spec :: Spec
spec =
  describe "Hinge Loss Tests" $ do
  
    it "Hinge loss single" $ do
      let x = LA.vector [1, 2]
          y = 1
          theta0 = -0.2
          m = hingeLossSingle x y theta theta0
      fromInteger (round (m * 10)) / 10 `shouldBe` 0.2

    it "Hinge loss full" $ do
      let x = (2><2) [1.0, 2.0, 1.0, 2.0]
          y = LA.vector [1.0, 1.0] 
          theta0 = -0.2
          m = hingeLossFull x y theta theta0
      fromInteger (round (m * 10)) / 10 `shouldBe` 0.2

    it "Perceptron single update" $ do
        let x = LA.vector [1, 2]
            y = 1
            theta0 = -1.5
        perceptronSingleStepUpdate x y theta theta0 `shouldBe` (LA.vector [0, 3], -0.5)

    it "Perceptron 1" $ do
        let x = (1><2) [1.0, 2.0] :: Matrix Double
            y = LA.vector [1]
            t = 1
        perceptron x y t `shouldBe` (LA.vector [1, 2], 1)

    it "Perceptron 2" $ do
        let x = (2><2) [1.0, 2.0, -1.0, 0] :: Matrix Double
            y = LA.vector [1, 1]
            t = 1
        perceptron x y t `shouldBe` (LA.vector [0, 2], 2)

    it "Perceptron 3" $ do
        let x = (1><2) [1.0, 2.0] :: Matrix Double
            y = LA.vector [1]
            t = 2
        perceptron x y t `shouldBe` (LA.vector [1, 2], 1)

    it "Perceptron 4" $ do
        let x = (2><2) [1.0, 2.0, -1.0, 0] :: Matrix Double
            y = LA.vector [1, 1]
            t = 2
        perceptron x y t `shouldBe` (LA.vector [0, 2], 2)

    it "Average Perceptron 1" $ do
        let x = (1><2) [1.0, 2.0] :: Matrix Double
            y = LA.vector [1]
            t = 1
        perceptron x y t `shouldBe` (LA.vector [1, 2], 1)

    it "Average Perceptron 2" $ do
        let x = (2><2) [-1.0, 0, 1.0, 2.0] :: Matrix Double
            y = LA.vector [1, 1]
            t = 1
        averagePerceptron x y t `shouldBe` (LA.vector [-0.5, 1], 1.5)

    it "Average Perceptron 3" $ do
        let x = (1><2) [1.0, 2.0] :: Matrix Double
            y = LA.vector [1]
            t = 2
        averagePerceptron x y t `shouldBe` (LA.vector [1, 2], 1)

    it "Average Perceptron 4" $ do
        let x = (2><2) [-1.0, 0, 1.0, 2.0] :: Matrix Double
            y = LA.vector [1, 1]
            t = 2
        averagePerceptron x y t `shouldBe` (LA.vector [-0.25, 1.5], 1.75)

    it "Pegasos single update 1" $ do
        let x = LA.vector [1, 2]
            y = 1
            lambda = 0.2
            eta = 0.1
            theta0 = -1.5
        pegasosSingleStepUpdate x y lambda eta theta theta0 `shouldBe` (LA.vector [-0.88, 1.18], -1.4)

    it "Pegasos single update 2" $ do
        let x = LA.vector [1, 1]
            y = 1
            lambda = 0.2
            eta = 0.1
            theta0 = 1
        pegasosSingleStepUpdate x y lambda eta theta theta0 `shouldBe` (LA.vector [-0.88, 1.08], 1.1)

    it "Pegasos single update 3" $ do
        let x = LA.vector [1, 2]
            y = 1
            lambda = 0.2
            eta = 0.1
            theta0 = -2
        pegasosSingleStepUpdate x y lambda eta theta theta0 `shouldBe` (LA.vector [-0.88, 1.18], -1.9)

    it "Pegasos 1" $ do
        let x = (1><2) [1, 2]
            y = LA.vector [1]
            t = 1
            lambda = 0.2
        pegasos x y t lambda `shouldBe` (LA.vector [1, 2], 1)

    it "Pegasos 2" $ do
        let x = (2><2) [1, 1, 1, 1]
            y = LA.vector [1, 1]
            t = 1
            lambda = 1
        pegasos x y t lambda `shouldBe` (LA.vector [1 - 1 / sqrt 2, 1 - 1 / sqrt 2], 1)
