module TypingIO where
import Parser
import PolymorphicTyping
import Control.Applicative (Alternative(..))


-- What is considered a valid variable name
validVar :: Parser Char String
validVar = do
  beginning <- letter
  rest <- many (letter <|> digit <|> char '\'' <|> char '_')
  return (beginning : rest)

-- What is considered a valid type variable name
validVarType :: Parser Char String
validVarType = validVar

var :: Parser Char Expr
var = Var <$> validVar

varType :: Parser Char Type
varType = TVar <$> validVarType

atomicType :: Parser Char Type 
atomicType = varType <|> forallType

-- Attempt to parse a type starting with forall quantifier. Liberal with spaces
forallType :: Parser Char Type
forallType = trimmingSpaces $ do
  _ <- char '@'
  arguments <- many (trimmingSpaces validVarType)
  _ <- char '.'
  _ <- spaces
  resultingType <- typeParser
  return $ foldr Forall resultingType arguments

-- Attempt to parse a type that is followed by a type arrow (->). Used to parse types with chaining arrows
leftArrowType :: Parser Char Type
leftArrowType = trimmingSpaces $ do
  leftType <- atomicType <|> insideParentheses typeParser
  _ <- spaces
  _ <- char '-' *> char '>'
  return leftType

-- Attempt to parse a type. It consists of zero or more types followed by an arrow and one last type followed by nothing
typeParser :: Parser Char Type
typeParser = trimmingSpaces $ do
  argumentTypes <- many leftArrowType
  resultingType <- atomicType <|> insideParentheses typeParser
  return $ foldr (:->) resultingType argumentTypes

-- Attempt to parse a type argument (type application uses {} syntax)
typeArgument :: Parser Char Type
typeArgument = trimmingSpaces $ do
  _ <- char '{'
  t <- typeParser
  _ <- char '}'
  return t

-- Attempt to parse a lambda. Note that brackets are not allowed around the argument
lambda :: Parser Char Expr
lambda = trimmingSpaces $ do
  _ <- char '\\'
  _ <- spaces
  arguments <- many ((,) <$> trimmingSpaces validVar <* char ':' <*> typeParser)
  _ <- char '.'
  _ <- spaces
  expr <- expression
  return $ foldr (\(argName, argType) rest -> Lam argName argType rest) expr arguments


--Attempt to parse a type abstraction. # symbol is used as a \Lambda
bigLambda :: Parser Char Expr 
bigLambda = trimmingSpaces $ do
  _ <- char '#'
  _ <- spaces
  arguments <- many (trimmingSpaces validVarType)
  _ <- char '.'
  _ <- spaces
  expr <- expression
  return $ foldr BigLam expr arguments

-- Expressions that don't have application or type application on the top level OR expression in brackets
atomicExpr :: Parser Char Expr
atomicExpr = trimmingSpaces (lambda <|> bigLambda <|> var <|> insideParentheses expression)

-- Used as a final parser in a chain. Asserts that the entire string has been consumed
assertEmpty :: Parser Char String 
assertEmpty = Parser f where
  f "" = Just ("", "")
  f _ = Nothing

-- Attempt to parse an expression. It consists of some atomic expressions and types chained with applications (:@) and type applications (:$)
expression :: Parser Char Expr
expression = trimmingSpaces $ do
  (Left firstExpr : xs) <- some (Left <$> atomicExpr <|>  Right <$> typeArgument)
  _ <- spaces
  return $ foldl binding firstExpr xs where
    binding result (Left expArg) = result :@ expArg 
    binding result (Right typeArg) = result :$ typeArg



-- Attempt to parse the environment. Expected syntax is <var1>:<type1>, <var2>:<type2>, ... , <varN>:<typeN>
environmentParser :: Parser Char Env
environmentParser = trimmingSpaces $ do
  let bindingParser = (,) <$> trimmingSpaces validVar <* char ':' <*> trimmingSpaces typeParser
  bindings <- many (bindingParser <* char ',')
  lastBinding <- singleOrEmptyList bindingParser
  return $ envFromList (bindings ++ lastBinding)
  
