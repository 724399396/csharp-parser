module CSharpParser (package, using, namespace, classP, typeP, variableDeclare, variableDeclareAndAssign, variableAssign, statements, expression, methodCall, atom, lambda, Package (..), Using (..), Namespace (..), Class (..), Modifier (..), Statement (..), Expression(..), Atom(..), CParser) where

import           Control.Monad        (join)
import           Data.List (intersperse)
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
data Package = Package (Maybe Input) Input deriving (Eq, Show)

-- instance Show Modifier where
--   show (Modifier is) = join $ intersperse " " is

-- instance Show Package where
--   show (Package n) = n

data Using = Using [Package] deriving (Eq, Show)
data Namespace = Namespace Name [Class] deriving (Eq, Show)

-- instance Show Using where
--   show (Using packages) = join $ intersperse "\n" $ map (\p -> "using " ++ show p ++ ";") packages

-- instance Show Namespace where
--   show (Namespace n e) = "namespace " ++ (show n) ++ "\n{\n" ++ show e ++ show "\n}"

data Class = Class Modifier Name [Parent] [Statement] deriving (Eq, Show)

-- instance Show Class where
--   show (Class ms n p b) = show ms ++ " " ++ n ++ "\n{\n" ++ (show b) ++ "\n}"

data Statement = VarDecl Modifier Type [Name]
               | VarDeclAndAssign Modifier Type [Name] Expression
               | VarAssign Name Expression
               | Exp Expression
  deriving (Eq, Show)

data Expression = Expression [Atom]
                | Lambda [Arg] [Statement]
                deriving (Eq, Show)

data Atom = Elem Name
          | MethodCall Name Expression
          | Op String
          | Literal String
          deriving (Eq, Show)

data CSharp = CSharp Using Namespace deriving (Eq, Show)

csharpStyle   :: LanguageDef st
csharpStyle =
  javaStyle { T.reservedNames = ["using", "class", "public", "private", "var", "namespace", "static", "readonly", "const"]
            , T.reservedOpNames = ["+", "-", "*", "/"]
            }

lexer :: T.TokenParser ()
lexer = T.makeTokenParser csharpStyle

identifier :: CParser String
identifier = T.identifier lexer
dot :: CParser String
dot = T.dot lexer
reserved :: String -> CParser ()
reserved = T.reserved lexer
semi :: CParser String
semi = T.semi lexer
braces :: CParser a -> CParser a
braces = T.braces lexer
reservedOp :: String -> CParser ()
reservedOp = T.reservedOp lexer
parens :: CParser a -> CParser a
parens = T.parens lexer
symbol :: String -> CParser String
symbol = T.symbol lexer
comma :: CParser String
comma = T.comma lexer
charLiteral :: CParser Char
charLiteral = T.charLiteral lexer
stringLiteral :: CParser String
stringLiteral = T.stringLiteral lexer
naturalOrFloat :: CParser (Either Integer Double)
naturalOrFloat = T.naturalOrFloat lexer
colon :: CParser String
colon = T.colon lexer
operator :: CParser String
operator = T.operator lexer
angles :: CParser a -> CParser a
angles = T.angles lexer

package :: CParser Input
package = (join . intersperse ".") <$> sepBy1 identifier dot

using :: CParser Using
using = Using <$> many singleUsing
  where
    singleUsing = reserved "using" >> Package <$> (option Nothing (Just <$> try (identifier <* reservedOp "="))) <*> package <* semi <?> "expect using"

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
  className' <- className
  parents <- option [] (colon >> sepBy1 className comma)
  statements' <- braces statements
  return $ Class (Modifier modifier) className' parents statements'
  where
    className = (++) <$> identifier <*> (option "" ((('<':) . (++ ">")) <$> angles identifier)) <?> "expect class"

-- body = many (noneOf "}")

typeP :: CParser Input
typeP = try (reserved "var" >> return "var") <|> ((join . intersperse ".") <$> sepBy1 identifier dot) <?> "expect type declare"

variableDeclareHelp :: CParser Statement
variableDeclareHelp = do
  m' <- many (choice $ map (\x -> reserved x >> return x) ["readonly","private","public","protected","const","static"])
  t' <- typeP
  vs' <- sepBy1 identifier comma
  return $ VarDecl (Modifier m') t' vs'

variableDeclare :: CParser Statement
variableDeclare = variableDeclareHelp <* semi

variableDeclareAndAssign :: CParser Statement
variableDeclareAndAssign = do
  (VarDecl m t v) <- variableDeclareHelp
  _ <- reservedOp "="
  e <- expression
  _ <- semi
  return $ VarDeclAndAssign m t v e

variableAssign :: CParser Statement
variableAssign = do
  v <- identifier
  _ <- reservedOp "="
  e <- expression
  _ <- semi
  return $ VarAssign v e

statements :: CParser [Statement]
statements = many statement
  where statement = try variableDeclare <|> try variableAssign <|> try variableDeclareAndAssign <|> (Exp <$> expression <* semi) <?> "expect statement"

expression :: CParser Expression
expression = try lambda <|> Expression <$> sepBy atom operator <?> "expect expression"

atom :: CParser Atom
atom = try methodCall
       <|> try (charLiteral >>= return . Literal . show)
       <|> try (stringLiteral >>= return . Literal . show)
       <|> try (naturalOrFloat >>= return . Literal . either show show)
       <|> (Elem <$> identifier)
       <?> "expect atom"

methodCall :: CParser Atom
methodCall = do i <- identifier
                p  <- parens expression
                return $ MethodCall i p

lambda :: CParser Expression
lambda = do args' <- args
            _ <- reservedOp "=>"
            s' <- try (braces statements) <|> statements
            return $ Lambda args' s'
  where args :: CParser [Arg]
        args = try (symbol "(" *> symbol ")" *> return [])
               <|> (\x -> [x]) <$> try identifier
               <|> parens (sepBy1 identifier comma)

csharp :: CParser CSharp
csharp = do using' <- using
            namespace' <- namespace
            return $ CSharp using' namespace'
