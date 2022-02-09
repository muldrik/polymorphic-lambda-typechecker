module TypingIO where
import Parser
import PolymorphicTyping
import Data.Char (isLower, isLetter ,isDigit, isSpace)
import Control.Applicative (Alternative(..))
import Control.Monad (guard)
import Data.Foldable(foldl1)

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
  rest <- many (letter <|> digit <|> char '\'')
  return (beginning : rest)

validType :: Parser Char String
validType = validVar

anyLex :: Parser Char String
anyLex = Parser f where
    f s = let ((result, rest) : _) = lex s in Just (rest, result)

var :: Parser Char Expr
var = Var <$> validVar

varType :: Parser Char Type 
varType = TVar <$> validType


lambda :: Parser Char Expr
lambda = do 
  _ <- char '\\'
  _ <- spaces 
  arguments <- many ((,) <$> validVar <* spaces <* char ':' <* spaces <*> varType <* spaces)
  _ <- char '.' 
  expr <- expression
  return $ foldr (\(argName, argType) rest -> Lam argName argType rest) expr arguments


atomicExpr :: Parser Char Expr
atomicExpr = do
  _ <- spaces
  openPars <- many (char '(' <* spaces)

  expr <- lambda <|> var

  _ <- spaces
  closingPars <- many (char ')' <* spaces)
  guard (length openPars == length closingPars)
  return expr


expression :: Parser Char Expr
expression = do
  atomics <- some atomicExpr
  return $ foldl1 (:@) atomics