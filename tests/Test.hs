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

  describe "parse type" $ do
    it "should parse var" $ do
      parse typeP "" "var" `shouldBe` Right "var"
    it "should parse explicit type" $ do
      parse typeP "" "Date" `shouldBe` Right "Date"
    it "should parse explicit type with full name" $ do
      parse typeP "" "System.Linq.IQueryable" `shouldBe` Right "System.Linq.IQueryable"


  describe "parse variable declare" $ do
    it "should parse single value declare" $ do
      parse variableDeclare "" "int a;" `shouldBe` Right (VarDecl (Modifier []) "int" ["a"])
    it "should parse value with const modifier" $ do
      parse variableDeclare "" "const long b;" `shouldBe` Right (VarDecl (Modifier ["const"]) "long" ["b"])
    it "should parse value with static modifier" $ do
      parse variableDeclare "" "static long c;" `shouldBe` Right (VarDecl (Modifier ["static"]) "long" ["c"])
    it "should parse multiple value with comma seperate" $ do
      parse variableDeclare "" "int d,e;" `shouldBe` Right (VarDecl (Modifier []) "int" ["d","e"])
    it "should fail when no semi" $ do
      parse variableDeclare "" "int a" `shouldSatisfy` isFailure


  describe "parse variable declare and assign" $ do
    it "should parse single value declare" $ do
      parse variableDeclareAndAssign "" "int a = 3;" `shouldBe` Right (VarDeclAndAssign (Modifier []) "int" ["a"] (Expression [Literal "3"]))
    it "should parse value with const modifier" $ do
      parse variableDeclareAndAssign "" "const long b = 4;" `shouldBe` Right (VarDeclAndAssign (Modifier ["const"]) "long" ["b"] (Expression [Literal "4"]))
    it "should parse value with static modifier" $ do
      parse variableDeclareAndAssign "" "static char c = 'c';" `shouldBe` Right (VarDeclAndAssign (Modifier ["static"]) "char" ["c"] (Expression [Literal "'c'"]))
    it "should parse multiple value with comma seperate" $ do
      parse variableDeclareAndAssign "" "string d,e = \"abc\";" `shouldBe` Right (VarDeclAndAssign (Modifier []) "string" ["d","e"] (Expression [Literal "\"abc\""]))
    it "should fail when no semi" $ do
      parse variableDeclareAndAssign "" "int a = 1" `shouldSatisfy` isFailure

  describe "parse variable assign" $ do
    it "should parse value assign" $ do
      parse variableAssign "" "a = 3;" `shouldBe` Right (VarAssign "a" (Expression [Literal "3"]))
    it "should fail when no semi" $ do
      parse variableAssign "" "a = 1" `shouldSatisfy` isFailure
    it "temporary not support multiple '='" $ do
      parse variableAssign "" "a = b = 3" `shouldSatisfy` isFailure

  describe "parse statements" $ do
    it "should support empty statements" $ do
      parse statements "" "" `shouldBe` Right []
    it "should parse variableDeclare, variableAssign, variableDeclareAndAssign, expression and lambda" $ do
      parse statements "" "int a; a = 1; int b = 2;Console.WriteLine();var c = () => {};" `shouldBe` Right ([VarDecl (Modifier []) "int" ["a"]
                                                                                                            , VarAssign "a" (Expression [Literal "1"])
                                                                                                            , VarDeclAndAssign (Modifier []) "int" ["b"] (Expression [Literal "2"])
                                                                                                           , Exp (Expression [Elem "Console", MethodCall "WriteLine" (Expression [])])
                                                                                                           , Exp (Lambda [] [])])
    it "should fail when miss semi" $ do
      parse statements "" "Console.WriteLine" `shouldSatisfy` isFailure

  describe "parse expression" $ do
    it "should parse directy method call" $ do
      parse expression "" "MethodA()" `shouldBe` Right (Expression [MethodCall "MethodA" (Expression [])])
    it "should parse multiple level method call" $ do
      parse expression "" "instanceA.MethodB().MethodC(instanceB)" `shouldBe` Right (Expression [Elem "instanceA", MethodCall "MethodB" (Expression []), MethodCall "MethodC" (Expression [Elem "instanceB"])])

  describe "parse atom" $ do
    it "should parse method" $ do
      parse atom "" "MethodA()" `shouldBe` Right (MethodCall "MethodA" (Expression []))
    it "should parse char literal" $ do
      parse atom "" "'a'" `shouldBe` Right (Literal "'a'")
    it "should parse string literal" $ do
      parse atom "" "\"a\"" `shouldBe` Right (Literal "\"a\"")
    it "should parse integer" $ do
      parse atom "" "1" `shouldBe` Right (Literal "1")
    it "should parse float" $ do
      parse atom "" "1.0" `shouldBe` Right (Literal "1.0")
    it "temporary not support long mark" $ do
      parse (atom >> eof) "" "1l" `shouldSatisfy` isFailure
    it "should parse identifier" $ do
      parse atom "" "Date" `shouldBe` Right (Elem "Date")

  describe "parse method call" $ do
    it "should parse without parameter" $ do
      parse methodCall "" "MethodA()" `shouldBe` Right (MethodCall "MethodA" (Expression []))
    it "should parse with parameter" $ do
      parse methodCall "" "MethodB(3)" `shouldBe` Right (MethodCall "MethodB" (Expression [Literal "3"]))

  describe "parse lambda" $ do
    it "should parse no argument and no body lambda" $ do
      parse lambda "" "() => {}" `shouldBe` Right (Lambda [] [])
    it "should parse one argument without parens" $ do
      parse lambda "" "arg => {}" `shouldBe` Right (Lambda ["arg"] [])
    it "should parse one argument with parens" $ do
      parse lambda "" "(arg) => {}" `shouldBe` Right (Lambda ["arg"] [])
    it "should parse multiple arguments with parens" $ do
      parse lambda "" "(arg1, arg2) => {}" `shouldBe` Right (Lambda ["arg1", "arg2"] [])
    it "should fail when multiple parameter no parens" $ do
      parse lambda "" "arg1, arg2 => {}" `shouldSatisfy` isFailure
    it "should success a single stentance" $ do
      parse lambda "" "() => 1;" `shouldBe` Right (Lambda [] [Exp $ Expression $ [Literal "1"]])
