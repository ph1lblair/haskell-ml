import Test.Hspec
import qualified SentimentAnalysisSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Sentiment Analysis Test" SentimentAnalysisSpec.spec
