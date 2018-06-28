module Main where

import Text.Parsec
import qualified Text.Parsec.Token as T
import Text.Parsec.Language (LanguageDef, javaStyle)
import Text.Parsec.Token
import Data.List (intersperse)
import Control.Monad (join)

type Input = String
type Name = String
type Body = String
newtype Package = Package Input deriving Show

data Exp = Using Package
         | Class Name Body deriving Show

csharpStyle   :: LanguageDef st
csharpStyle =
  javaStyle {reservedNames = ["using", "class", "public", "private", "var", "namespace", "static", "readonly"]}

lexer :: T.TokenParser ()
lexer = T.makeTokenParser csharpStyle

package :: Parsec Input () String
package = (join . intersperse ".") <$> sepBy (T.identifier lexer) (T.dot lexer)

usings :: Parsec Input () [Package]
usings = many using
  where
    using = (T.reserved lexer "using") >> Package <$> package <* (T.semi lexer)

namespace :: Parsec Input () a -> Parsec Input () a
namespace body = (T.reserved lexer "namespace") *> package *> T.braces lexer body

classP :: Parsec Input () Exp
classP = do
  try $ T.reserved lexer "public"
  try $ T.reserved lexer "private"
  T.reserved lexer "class"
  className <- T.identifier lexer
  b <- T.braces lexer body
  return $ Class className b

-- body = many (noneOf "}")

typeP :: Parsec Input () ()
typeP = try (T.reserved lexer "var") <|> (T.identifier lexer >> return ())

variableDeclare = typeP *> T.identifier lexer

variableDeclareAndAssign = do
  v <- variableDeclare
  T.reservedOp lexer "="
  e <- expression

body = undefined

methodCall :: Parsec Input () Exp
methodCall

expression :: Parsec Input () Exp
expression = 

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

main :: IO ()
main = do
  putStrLn "hello world"
