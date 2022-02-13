{-# LANGUAGE RankNTypes, InstanceSigs, NoMonomorphismRestriction, ScopedTypeVariables #-}
module Parser where

import Control.Applicative (Alternative(..))
import Control.Monad (guard)
import Data.Char (isLower, isLetter ,isDigit, isSpace)
import Data.List (stripPrefix)


--Applicative parser. Base definitions to easily define domain-specific functon
--To parse string, choose Char as a tok
newtype Parser tok a = 
  Parser { runParser :: [tok] ->  Maybe ([tok],a) }

--Assert that the parser consumed the entire input
parserResult :: Parser tok a -> [tok] -> Maybe a
parserResult p s = do 
  (remainder, result) <- runParser p s
  guard $ null remainder 
  return result


instance Functor (Parser tok) where
  fmap :: (a -> b) -> Parser tok a -> Parser tok b
  fmap g (Parser p) = Parser f where
    f xs = case p xs of 
      Nothing      -> Nothing
      Just (cs, c) -> Just (cs, g c)

-- Read the input with the first parser, read the remainder with the second and apply the results together
instance Applicative (Parser tok) where
  pure :: a -> Parser tok a
  pure x = Parser $ \s -> Just (s, x)

  (<*>) :: Parser tok (a -> b) -> Parser tok a -> Parser tok b
  Parser u <*> Parser v = Parser f where
    f xs = case u xs of 
      Nothing       -> Nothing
      Just (xs', g) -> case v xs' of 
        Nothing        -> Nothing
        Just (xs'', x) -> Just (xs'', g x)


instance Alternative (Parser tok) where
  empty :: Parser tok a
  empty = Parser $ \_ -> Nothing

-- Try until the parser succeedes  
  (<|>) :: Parser tok a -> Parser tok a -> Parser tok a
  Parser u <|> Parser v = Parser f where 
    f xs = case u xs of
      Nothing -> v xs
      z       -> z

-- Read the input with a sequence of parsers, possibly accumulating the results
instance Monad (Parser tok) where
  return = pure
  (>>=) :: Parser tok a -> (a -> Parser tok b) -> Parser tok b
  (Parser f) >>= g = Parser h where
    h s = case f s of
      Nothing -> Nothing
      Just (rest, result) -> runParser (g result) rest


instance MonadFail (Parser tok) where
  fail _ = empty

satisfy :: (tok -> Bool) -> Parser tok tok
satisfy pr = Parser f where
  f (c:cs) | pr c  = Just (cs,c)
  f _              = Nothing

lower :: Parser Char Char
lower = satisfy isLower

letter :: Parser Char Char
letter = satisfy isLetter

--Read a target char
char :: Char -> Parser Char Char
char c = satisfy (== c)

digit :: Parser Char Char
digit = satisfy isDigit

lowers :: Parser Char String
lowers = (:) <$> lower <*> lowers <|> pure ""

spaces :: Parser Char String
spaces = many (satisfy isSpace)

-- Read a target string
str :: String -> Parser Char String
str goal = Parser f where
  f s = do
    rest <- stripPrefix goal s
    return (rest, goal)

-- Trim spaces, read surrounding brackets and apply given parser to the inside
insideParentheses :: Parser Char a -> Parser Char a
insideParentheses p = trimmingSpaces $ do
  _ <- char '(' <* spaces

  result <- p

  _ <- spaces
  _ <- char ')'
  return result

-- Trim the spaces, apply parser, then trim remainding spaces
trimmingSpaces :: Parser Char a -> Parser Char a
trimmingSpaces p = do
  _ <- spaces
  result <- p
  _ <- spaces
  return result

-- Useful when chaining with some/many and failed parsing is not considered an error
singleOrEmptyList :: Parser Char a -> Parser Char [a]
singleOrEmptyList p = Parser f where
  f s = case runParser p s of
    Just (rest, res) -> Just (rest, [res])
    Nothing -> Just (s, [])