import Test.Hspec
import CSharpParser
import Text.Parsec

isFailure :: Either a b -> Bool
isFailure (Left _) = True
isFailure _ = False

main :: IO ()
main = hspec $ do
  describe "parse package" $ do
    it "should parse no dot package name" $ do
      parse package "" "System" `shouldBe` Right (Package "System")
    it "should parse dot split pckage name" $ do
      parse package "" "System.Linq" `shouldBe` Right (Package "System.Linq")

  describe "parse using" $ do
    it "should parse single line using" $ do
      parse using "" "using System;" `shouldBe` Right (Using [Package "System"])
    it "should parse multiple line using" $ do
      parse using "" "using System.Linq;\n\tusing System; " `shouldBe` Right (Using [Package "System.Linq", Package "System"])
    it "should fail when no semi" $ do
      parse using "" "using System.Linq" `shouldSatisfy` isFailure

  describe "parse namespace" $ do
    it "should parse namespace with underscore" $ do
      parse namespace "" "namespace Test.ResourceTests.search_api{}" `shouldBe` Right (Namespace (Package "Test.ResourceTests.search_api") [])
    it "should fail when has input not consume" $ do
      parse namespace "" "namespace Test.ResourceTests.search_api{}n" `shouldSatisfy` isFailure

  describe "parse class" $ do
    it "should parse class with single modifier" $ do
      parse classP "" "private class A{}" `shouldBe` Right (Class (Modifier ["private"]) "A" [] [])
    it "should parse class with multiple modifier" $ do
      parse classP "" "public static class A{}" `shouldBe` Right (Class (Modifier ["public", "static"]) "A" [] [])
    it "should parse class with parent" $ do
      parse classP "" "class A : Parent{}" `shouldBe` Right (Class (Modifier []) "A" ["Parent"] [])
    it "should parse class with parents" $ do
      parse classP "" "class A : Parent1, Parent2{}" `shouldBe` Right (Class (Modifier []) "A" ["Parent1", "Parent2"] [])
    it "should fail when no word 'class'" $ do
      parse classP "" "private A{}" `shouldSatisfy` isFailure
    it "should fail when no braces" $ do
      parse classP "" "class A" `shouldSatisfy` isFailure



