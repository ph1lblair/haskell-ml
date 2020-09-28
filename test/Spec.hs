import Test.Hspec
import qualified SentimentAnalysisSpec
import qualified AutomatedReviewAnalyzerSpec
import qualified DigitRecognitionSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = 
--  describe "Sentiment Analysis Test" SentimentAnalysisSpec.spec
--  describe "Automated Review Analyzer" AutomatedReviewAnalyzerSpec.spec
  describe "Digit Recognition" DigitRecognitionSpec.spec
