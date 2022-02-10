module TypingIO where
import Parser
import PolymorphicTyping
import Data.Char (isLower, isLetter ,isDigit, isSpace)
import Control.Applicative (Alternative(..))

lower :: Parser Char Char
lower = satisfy isLower

letter :: Parser Char Char
letter = satisfy isLetter

char :: Char -> Parser Char Char
char c = satisfy (== c)

digit :: Parser Char Char
digit = satisfy isDigit

lowers :: Parser Char String
lowers = (:) <$> lower <*> lowers <|> pure ""

spaces :: Parser Char String
spaces = many (satisfy isSpace)

lexeme :: String -> Parser Char String
lexeme expected = Parser f where
    f s = let ((actual, rest) : _) = lex s in
        if expected == actual then Just (actual, rest) else Nothing

validVar :: Parser Char String
validVar = do
  beginning <- letter
  rest <- many (letter <|> digit <|> char '\'' <|> char '_')
  return (beginning : rest)

validVarType :: Parser Char String
validVarType = validVar

anyLex :: Parser Char String
anyLex = Parser f where
    f s = let ((result, rest) : _) = lex s in Just (rest, result)

var :: Parser Char Expr
var = Var <$> validVar

varType :: Parser Char Type
varType = TVar <$> validVarType

typeLambda :: Parser Char Expr
typeLambda = do
  _ <- char '$'
  _ <- spaces
  arguments <- many (validVarType <* spaces)
  _ <- char '.'
  expr <- expression
  return $ foldr BigLam expr arguments


trimmingSpaces :: Parser Char a -> Parser Char a
trimmingSpaces p = do
  _ <- spaces
  result <- p
  _ <- spaces
  return result

lambda :: Parser Char Expr
lambda = trimmingSpaces $ do
  _ <- char '\\'
  _ <- spaces
  arguments <- many ((,) <$> trimmingSpaces validVar <* char ':' <*> typeParser)
  _ <- char '.'
  _ <- spaces
  expr <- expression
  return $ foldr (\(argName, argType) rest -> Lam argName argType rest) expr arguments

insideParentheses :: Parser Char a -> Parser Char a
insideParentheses p = trimmingSpaces $ do
  _ <- char '(' <* spaces

  result <- p

  _ <- spaces
  _ <- char ')'
  return result

atomicExpr :: Parser Char Expr
atomicExpr = trimmingSpaces (lambda <|> var <|> insideParentheses expression)


leftArrowType :: Parser Char Type
leftArrowType = trimmingSpaces $ do
  leftType <- varType <|> insideParentheses typeParser
  _ <- spaces
  _ <- char '-' *> char '>'
  return leftType


typeParser :: Parser Char Type
typeParser = trimmingSpaces $ do
  argumentTypes <- many leftArrowType
  resultingType <- varType <|> insideParentheses typeParser
  return $ foldr (:->) resultingType argumentTypes

typeArgument :: Parser Char Type
typeArgument = trimmingSpaces $ do
  _ <- char '{'
  t <- typeParser
  _ <- char '}'
  return t

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