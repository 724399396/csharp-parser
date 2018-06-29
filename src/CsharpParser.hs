module CSharpParser (usings, Package (..)) where

import Text.Parsec
import qualified Text.Parsec.Token as T
import Text.Parsec.Language (LanguageDef, javaStyle)
import Data.List (intersperse)
import Control.Monad (join)

type Input = String
type Name = String
type Body = String
newtype Modifier = Modifier [Input] deriving Eq
newtype Package = Package Input deriving Eq

instance Show Package where
  show (Package n) = n

data Exp = Using [Package]
         | Namespace Package Exp
         | Class [Name] Name Body deriving Eq

instance Show Exp where
  show (Using packages) = join $ intersperse "\n" $ map (\p -> "using " ++ show p ++ ";") packages
  show (Namespace n e) = "namespace " ++ (show n) ++ "\n{\n" ++ show e ++ show "\n}"
  show (Class ms n b) = (join $ intersperse " " (map show ms)) ++ " " ++ " " ++ n ++ "\n{\n" ++ b ++ "\n}"

csharpStyle   :: LanguageDef st
csharpStyle =
  javaStyle {T.reservedNames = ["using", "class", "public", "private", "var", "namespace", "static", "readonly"]}

lexer :: T.TokenParser ()
lexer = T.makeTokenParser csharpStyle

identifier :: Parsec String () String
identifier = T.identifier lexer
dot :: Parsec String () String
dot = T.dot lexer
reserved :: String -> Parsec String () ()
reserved = T.reserved lexer
semi :: Parsec String () String
semi = T.semi lexer
braces :: Parsec String () a -> Parsec String () a
braces = T.braces lexer
reservedOp :: String -> Parsec String () ()
reservedOp = T.reservedOp lexer

package :: Parsec String () String
package = (join . intersperse ".") <$> sepBy identifier dot

usings :: Parsec Input () [Package]
usings = many using
  where
    using = reserved "using" >> Package <$> package <* semi

namespace :: Parsec Input () a -> Parsec Input () a
namespace c = reserved "namespace" *> package *> braces c

classP :: Parsec Input () Exp
classP = do
  try $ (reserved "public" <|> reserved "private")
  reserved "class"
  className <- identifier
  b <- braces body
  return $ Class className b

-- body = many (noneOf "}")

typeP :: Parsec Input () ()
typeP = try (reserved "var") <|> (identifier >> return ())

variableDeclare :: Parsec Input () String
variableDeclare = typeP *> identifier

variableDeclareAndAssign :: Parsec Input () ()
variableDeclareAndAssign = do
  v <- variableDeclare
  _ <- reservedOp "="
  e <- expression
  return ()

body :: a
body = undefined

methodCall :: Parsec Input () Exp
methodCall = undefined

expression :: Parsec Input () Exp
expression = undefined

-- lambda :: Parsec Input ParseInfomation Exp
-- lambda = do
--   spaces
--   string "()"
--   spaces
--   string "=>"
--   spaces
--   try $ char '{'
--   spaces
--   b <-
--   try $ char '}'
--   spaces
--   char ';'
--   return b

-- declare :: Parsec Input ClassNest Exp
-- declare = do
--   skipMany (letter <|> space)
--   char '='
--   lambda
