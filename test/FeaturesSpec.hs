module FeaturesSpec where

import Numeric.LinearAlgebra as LA
import Test.Hspec
import Features

spec :: Spec
spec =
  describe "Dimension Reuduction Tests" $ do
  
    it "Column mean" $ do
        let m = (4 >< 3) [1, 2, 3, 2, 4, 6, 3, 6, 9, 4, 8, 12]
            expected = fromList [2.5, 5, 7.5]
            res = mean m
        res `shouldBe` expected

    it "Centre data" $ do
        let m = (4 >< 3) [1, 2, 3, 2, 4, 6, 3, 6, 9, 4, 8, 12]
            expected = (4 >< 3) [-1.5, -3, -4.5, -0.5, -1, -1.5, 0.5, 1, 1.5, 1.5, 3, 4.5]
            res = centreData m
        res `shouldBe` expected

    it "Principal components" $ do
        let x = (4 >< 3) [1, 2, 3, 2, 4, 6, 3, 6, 9, 4, 8, 12]
            expected = (3 >< 3) [0.26726124191242434,  -0.7554721158378965, -0.5981917006803421,
                  0.5345224838248487, 0.632740724488323, -0.5602900051399329,
                  0.8017837257372732, -0.17000311104624982, 0.5729239036534026 ]
            res = principalComponents x
        res `shouldBe` expected

    it "Project onto PC" $ do
        let x = (4 >< 3) [1, 2, 3, 2, 4, 6, 3, 6, 9, 4, 8, 12]
            n = 2
            expected = (4 >< 2) [-5.612486080160911, 0, -1.8708286933869707, 0, 1.8708286933869707, 0, 5.612486080160911, 0]
            res1 = projectOntoPC x (principalComponents x) n
        res1 `shouldBe` expected

    it "Cubic Features 1" $ do
        let x = (2 >< 2) [1, 2, 3, 4]
            expected = (2 >< 10) [  1.0,  8.0, 1.0, 6.928203230275509, 3.4641016151377544, 3.4641016151377544, 6.928203230275509, 
                1.7320508075688772, 1.7320508075688772, 4.898979485566356, 27.0, 64.0, 1.0, 83.13843876330611, 62.35382907247958,
                6.928203230275509, 27.712812921102035,  5.196152422706632, 15.588457268119894, 29.393876913398135 ]
            res1 = cubicFeatures x 
        res1 `shouldBe` expected

    it "Cubic Features 2" $ do
        let x = LA.vector [1, 2, 3, 4, 5]
            expected = [14.696938456699067,19.595917942265423,24.49489742783178,29.393876913398135,36.74234614174767,
                        48.98979485566356,58.78775382679627,73.48469228349533,97.97958971132712,146.96938456699067]
            res1 = crossTerms x 
        res1 `shouldBe` expected

    it "Cubic Features 3" $ do
        let x = (1 >< 4) [1, 2, 3, 4]
            expected = (1 >< 35) [1.0, 8.0, 27.0, 64.0, 1.0, 6.928203230275509, 3.4641016151377544, 31.17691453623979,
                20.784609690826528, 83.13843876330611, 62.35382907247958, 6.928203230275509, 27.712812921102035,
                15.588457268119894, 5.196152422706632, 55.42562584220407, 27.712812921102035, 5.196152422706632,
                15.588457268119894, 27.712812921102035, 6.928203230275509, 3.4641016151377544, 6.928203230275509,
                1.7320508075688772, 1.7320508075688772, 4.898979485566356, 14.696938456699067, 29.393876913398135,
                7.348469228349534, 19.595917942265423, 9.797958971132712, 14.696938456699067, 19.595917942265423,
                29.393876913398135, 58.78775382679627]
            res1 = cubicFeatures x 
        res1 `shouldBe` expected
