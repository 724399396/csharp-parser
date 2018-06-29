import Test.Hspec
import CSharpParser
import Text.Parsec

isFailure :: Either a b -> Bool
isFailure (Left _) = True
isFailure _ = False

main :: IO ()
main = hspec $ do
  describe "parse using" $ do
    it "should parse single line using" $ do
      parse usings "" "using System;" `shouldBe` Right [Package "System"]
    it "should parse multiple line using" $ do
      parse usings "" "using System.Linq;\n\tusing System; " `shouldBe` Right [Package "System.Linq", Package "System"]
    it "should fail when no semi" $ do
      parse usings "" "using System.Linq" `shouldSatisfy` isFailure

