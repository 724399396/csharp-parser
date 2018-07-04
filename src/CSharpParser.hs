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
data Using = Using [Package] deriving (Eq, Show)
data Namespace = Namespace Name [Class] deriving (Eq, Show)

data Class = Class Modifier Name [Parent] [Statement] deriving (Eq, Show)

data Statement = VarDecl Modifier Type [Name]
               | VarDeclAndAssign Modifier Type [Name] Expression
               | VarAssign Name Expression
               | Exp Expression
               | Cls Class
  deriving (Eq, Show)

data Expression = Expression [Atom]
                | Lambda [Arg] [Statement]
                deriving (Eq, Show)

data Atom = Elem Name
          | MethodCall Name [Expression]
          | Op String
          | Literal String
          | New Name [Expression] [Statement]
          deriving (Eq, Show)

data CSharp = CSharp Using Namespace deriving (Eq, Show)

csharpStyle   :: LanguageDef st
csharpStyle =
  javaStyle { T.reservedNames = ["using", "class", "public", "private", "var", "namespace", "static", "readonly", "const", "new"]
            , T.reservedOpNames = ["+", "-", "*", "/", "="]
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
commaSep :: CParser a -> CParser [a]
commaSep = T.commaSep lexer

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

className :: CParser Name
className = (++) <$> identifier <*> (option "" ((('<':) . (++ ">")) <$> angles identifier)) <?> "expect class"

classP :: CParser Class
classP = do
  modifier <- many $ choice $ map (\x -> reserved x >> return x) ["public", "private", "static"]
  reserved "class"
  className' <- className
  parents <- option [] (colon >> sepBy1 className comma)
  statements' <- braces (many (try statement <|> (Cls <$> classP)))
  return $ Class (Modifier modifier) className' parents statements'

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
  return $ VarAssign v e

statement :: CParser Statement
statement = try variableDeclare <|> try (variableAssign <* semi) <|> try variableDeclareAndAssign <|> (Exp <$> expression <* semi) <?> "expect statement"

objectInitilize :: CParser [Statement]
objectInitilize = commaSep (try variableAssign <|> (Exp <$> expression))

statements :: CParser [Statement]
statements = many statement

expression :: CParser Expression
expression = try lambda <|> Expression <$> sepBy atom operator <?> "expect expression"

atom :: CParser Atom
atom = try newInstance
       <|> try methodCall
       <|> try (charLiteral >>= return . Literal . show)
       <|> try (stringLiteral >>= return . Literal . show)
       <|> try (naturalOrFloat >>= return . Literal . either show show)
       <|> (Elem <$> identifier)
       <?> "expect atom"
  where newInstance e= do
          reserved "new"
          className' <- (className <|> (reserved "[]" >> return "[]"))
          parameters' <- option [] (parens (commaSep expression))
          initilize' <- option [] (braces objectInitilize)
          return $ New className' parameters' initilize'


methodCall :: CParser Atom
methodCall = do i <- (++) <$> identifier <*> (option "" ((('<':) . (++ ">")) <$> angles identifier)) <?> "expect method"
                p  <- parens (commaSep expression)
                return $ MethodCall i p

lambda :: CParser Expression
lambda = do args' <- args
            _ <- reservedOp "=>"
            s' <- try (braces statements) <|> ((:[]) <$> statement)
            return $ Lambda args' s'
  where args :: CParser [Arg]
        args = try (symbol "(" *> symbol ")" *> return [])
               <|> (\x -> [x]) <$> try identifier
               <|> parens (sepBy1 identifier comma)

csharp :: CParser CSharp
csharp = do using' D<- using
            namespace' <- namespace
            return $ CSharp using' namespace'

originFormatList :: (OriginFormat a) => [a] -> String -> String
originFormatList xs terminator = (join $ intersperse terminator $ map originFormat xs)

class OriginFormat a where
  originFormat :: a -> String

instance OriginFormat CSharp where
  originFormat (CSharp u n) = (originFormat u) ++ "\n" ++ (originFormat n)

instance OriginFormat Using where
  originFormat (Using packages) = join $ intersperse "\n" $ map (\p -> "using " ++ originFormat p ++ ";") packages

instance OriginFormat Package where
  originFormat (Package Nothing n) = n
  originFormat (Package (Just a) n) = a ++ " = " ++ n

instance OriginFormat Namespace where
  originFormat (Namespace n e) = "namespace " ++ n ++ "\n{\n" ++ originFormatList e "\n" ++ "\n}"

instance OriginFormat Class where
  originFormat (Class ms n [] b) =  originFormat ms ++ " " ++ n ++ "\n{\n" ++ (originFormatList b "\n") ++ "\n}"
  originFormat (Class ms n xs b) =  originFormat ms ++ " " ++ n ++ ":" ++ (intersperse ", " xs) ++ "\n{\n" ++ (originFormatList b "\n") ++ "\n}"

instance OriginFormat Modifier where
  originFormat (Modifier is) = join $ intersperse " " is

instance OriginFormat Statement where
  originFormat (VarDecl m t n) = originFormat m ++ " " ++ originFormat t ++ " " ++ intersperse ", " n
  originFormat (VarDeclAndAssign m t n e) = originFormat m ++ " " ++ originFormat t ++ " " ++ (intersperse ", " n) ++ " = "  ++ originFormatList e ";\n"

--                | VarAssign Name Expression
--                | Exp Expression
--                | Cls Class
--   deriving (Eq, Show)

-- data Expression = Expression [Atom]
--                 | Lambda [Arg] [Statement]
--                 deriving (Eq, Show)

-- data Atom = Elem Name
--           | MethodCall Name [Expression]
--           | Op String
--           | Literal String
--           | New Name [Expression] [Statement]
--           deriving (Eq, Show)
