module CSharpParser (package, using, namespace, classP, Package (..), Using (..), Namespace (..), Class (..), Modifier (..)) where

import           Control.Monad        (join)
import           Text.Parsec
import           Text.Parsec.Language (LanguageDef, javaStyle)
import qualified Text.Parsec.Token    as T

type Input = String
type Name = String
type Type = String
type Arg = String
type Parent = String
type CParser a = Parsec Input () a
newtype Modifier = Modifier [Input] deriving (Eq, Show)
newtype Package = Package Input deriving (Eq, Show)

-- instance Show Modifier where
--   show (Modifier is) = join $ intersperse " " is

-- instance Show Package where
--   show (Package n) = n

data Using = Using [Package] deriving (Eq, Show)
data Namespace = Namespace Package [Class] deriving (Eq, Show)

-- instance Show Using where
--   show (Using packages) = join $ intersperse "\n" $ map (\p -> "using " ++ show p ++ ";") packages

-- instance Show Namespace where
--   show (Namespace n e) = "namespace " ++ (show n) ++ "\n{\n" ++ show e ++ show "\n}"

data Class = Class Modifier Name [Parent] [Statement] deriving (Eq, Show)

-- instance Show Class where
--   show (Class ms n p b) = show ms ++ " " ++ n ++ "\n{\n" ++ (show b) ++ "\n}"

data Statement = VarDecl Type Name
               | VarDeclAndAssign Type Name Expression
               | VarAssign Name Expression
               | Exp Expression
               | Lambda [Arg] [Statement]
  deriving (Eq, Show)

data Expression = Expression [Atom] deriving (Eq, Show)

data Atom = Elem Name
          | MethodCall Name Expression
          | Op String
          deriving (Eq, Show)

data CSharp = CSharp Using Namespace deriving (Eq, Show)

csharpStyle   :: LanguageDef st
csharpStyle =
  javaStyle { T.reservedNames = ["using", "class", "public", "private", "var", "namespace", "static", "readonly"]
            , T.reservedOpNames = ["+", "-", "*", "/", "=>"]
            }

lexer :: T.TokenParser ()
lexer = T.makeTokenParser csharpStyle

identifier = T.identifier lexer
dot = T.dot lexer
reserved = T.reserved lexer
semi = T.semi lexer
braces = T.braces lexer
reservedOp = T.reservedOp lexer
parens = T.parens lexer
symbol = T.symbol lexer
comma = T.comma lexer

package :: CParser Package
package = (Package . join) <$> many1 (identifier <|> dot)

using :: CParser Using
using = Using <$> many singleUsing
  where
    singleUsing = reserved "using" >> package <* semi

namespace :: CParser Namespace
namespace = do
  reserved "namespace"
  n <- package
  b <- braces (many classP)
  eof
  return $ Namespace n b

classP :: CParser Class
classP = do
  modifier <- many $ choice $ map (\x -> reserved x >> return x) ["public", "private", "static"]
  reserved "class"
  className <- identifier
  parents <- option [] (reservedOp ":" >> sepBy1 identifier comma)
  statements' <- braces statements
  return $ Class (Modifier modifier) className parents statements'

-- body = many (noneOf "}")

typeP :: CParser Input
typeP = try (reserved "var" >> return "var") <|> identifier

variableDeclare :: CParser Statement
variableDeclare = VarDecl <$> typeP <*> identifier <* semi

variableDeclareAndAssign :: CParser Statement
variableDeclareAndAssign = do
  (VarDecl t n) <- variableDeclare
  _ <- reservedOp "="
  e <- expression
  return $ VarDeclAndAssign t n e

variableAssign :: CParser Statement
variableAssign = do
  v <- identifier
  _ <- reservedOp "="
  e <- expression
  return $ VarAssign v e

statement :: CParser Statement
statement = try variableDeclare <|> try variableAssign <|> try variableDeclareAndAssign <|> try (Exp <$> expression) <|> lambda

statements :: CParser [Statement]
statements = many statement

expression :: CParser Expression
expression = Expression <$> many1 atom <* semi

atom :: CParser Atom
atom = try methodCall <|> (Elem <$> identifier) <|> (choice $ map (\x -> reservedOp x >> return (Op x)) ["+","-","*","/"])

methodCall :: CParser Atom
methodCall = do i <- identifier
                p  <- parens expression
                return $ MethodCall i p

lambda :: CParser Statement
lambda = do args' <- args
            _ <- reservedOp "=>"
            s' <- ((:[]) <$> statement) <|> statements
            return $ Lambda args' s'
  where args :: CParser [Arg]
        args = try (symbol "(" *> symbol ")" *> return [])
               <|> (\x -> [x]) <$> try identifier
               <|> parens (sepBy1 identifier comma)

csharp :: CParser CSharp
csharp = do using' <- using
            namespace' <- namespace
            return $ CSharp using' namespace'
