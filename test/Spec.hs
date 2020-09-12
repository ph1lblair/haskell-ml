import Test.Hspec
import qualified SentimentAnalysisSpec
import qualified AutomatedReviewAnalyzerSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do 
  describe "Sentiment Analysis Test" SentimentAnalysisSpec.spec
  describe "Automated Review Analyzer" AutomatedReviewAnalyzerSpec.spec
