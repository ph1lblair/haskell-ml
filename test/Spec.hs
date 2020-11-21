import Test.Hspec
import qualified SentimentAnalysisSpec
import qualified AutomatedReviewAnalyzerSpec
import qualified DigitRecognitionSpec
import qualified FeaturesSpec
import qualified NNSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = 
--  describe "Sentiment Analysis Test" SentimentAnalysisSpec.spec
--  describe "Automated Review Analyzer" AutomatedReviewAnalyzerSpec.spec
--  describe "Digit Recognition" DigitRecognitionSpec.spec
--  describe "PCA Dimension Reduction" FeaturesSpec.spec
  describe "Neural Network" NNSpec.spec
