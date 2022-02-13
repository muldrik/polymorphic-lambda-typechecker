module TypingIO where
import Parser
import PolymorphicTyping
import Control.Applicative (Alternative(..))


validVar :: Parser Char String
validVar = do
  beginning <- letter
  rest <- many (letter <|> digit <|> char '\'' <|> char '_')
  return (beginning : rest)

validVarType :: Parser Char String
validVarType = validVar

var :: Parser Char Expr
var = Var <$> validVar

varType :: Parser Char Type
varType = TVar <$> validVarType


atomicType :: Parser Char Type 
atomicType = varType <|> forallType

forallType :: Parser Char Type
forallType = trimmingSpaces $ do
  _ <- char '@'
  arguments <- many (trimmingSpaces validVarType)
  _ <- char '.'
  _ <- spaces
  resultingType <- typeParser
  return $ foldr Forall resultingType arguments

leftArrowType :: Parser Char Type
leftArrowType = trimmingSpaces $ do
  leftType <- atomicType <|> insideParentheses typeParser
  _ <- spaces
  _ <- char '-' *> char '>'
  return leftType



typeParser :: Parser Char Type
typeParser = trimmingSpaces $ do
  argumentTypes <- many leftArrowType
  resultingType <- atomicType <|> insideParentheses typeParser
  return $ foldr (:->) resultingType argumentTypes

typeArgument :: Parser Char Type
typeArgument = trimmingSpaces $ do
  _ <- char '{'
  t <- typeParser
  _ <- char '}'
  return t


lambda :: Parser Char Expr
lambda = trimmingSpaces $ do
  _ <- char '\\'
  _ <- spaces
  arguments <- many ((,) <$> trimmingSpaces validVar <* char ':' <*> typeParser)
  _ <- char '.'
  _ <- spaces
  expr <- expression
  return $ foldr (\(argName, argType) rest -> Lam argName argType rest) expr arguments


bigLambda :: Parser Char Expr 
bigLambda = trimmingSpaces $ do
  _ <- char '#'
  _ <- spaces
  arguments <- many (trimmingSpaces validVarType)
  _ <- char '.'
  _ <- spaces
  expr <- expression
  return $ foldr BigLam expr arguments


atomicExpr :: Parser Char Expr
atomicExpr = trimmingSpaces (lambda <|> bigLambda <|> var <|> insideParentheses expression)


assertEmpty :: Parser Char String 
assertEmpty = Parser f where
  f "" = Just ("", "")
  f _ = Nothing

expression :: Parser Char Expr
expression = trimmingSpaces $ do
  (Left firstExpr : xs) <- some (Left <$> atomicExpr <|>  Right <$> typeArgument)
  _ <- spaces
  return $ foldl binding firstExpr xs where
    binding result (Left expArg) = result :@ expArg 
    binding result (Right typeArg) = result :$ typeArg




environmentParser :: Parser Char Env
environmentParser = trimmingSpaces $ do
  let bindingParser = (,) <$> trimmingSpaces validVar <* char ':' <*> trimmingSpaces typeParser
  bindings <- many (bindingParser <* char ',')
  lastBinding <- singleOrEmptyList bindingParser
  return $ envFromList (bindings ++ lastBinding)
  
