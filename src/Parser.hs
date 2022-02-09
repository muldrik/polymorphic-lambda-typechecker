{-# LANGUAGE RankNTypes, InstanceSigs, NoMonomorphismRestriction, ScopedTypeVariables #-}
module Parser where

import Control.Applicative (Alternative(..))

newtype Parser tok a = 
  Parser { runParser :: [tok] ->  Maybe ([tok],a) }

satisfy :: (tok -> Bool) -> Parser tok tok
satisfy pr = Parser f where
  f (c:cs) | pr c  = Just (cs,c)
  f _              = Nothing


instance Functor (Parser tok) where
  fmap :: (a -> b) -> Parser tok a -> Parser tok b
  fmap g (Parser p) = Parser f where
    f xs = case p xs of 
      Nothing      -> Nothing
      Just (cs, c) -> Just (cs, g c)


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
  
  (<|>) :: Parser tok a -> Parser tok a -> Parser tok a
  Parser u <|> Parser v = Parser f where 
    f xs = case u xs of
      Nothing -> v xs
      z       -> z


instance Monad (Parser tok) where
  return = pure
  (>>=) :: Parser tok a -> (a -> Parser tok b) -> Parser tok b
  (Parser f) >>= g = Parser h where
    h s = case f s of
      Nothing -> Nothing
      Just (rest, result) -> runParser (g result) rest


instance MonadFail (Parser tok) where
  fail _ = empty


-- Альтернативы

{-
СЕМАНТИКА
empty - парсер, всегда возвращающий неудачу;
<|> - пробуем первый, при неудаче пробуем второй на исходной строке.
-}


        
{-
> runParser  (char 'A' <|> char 'B') "ABC"
Just ("BC",'A')
> runParser  (char 'A' <|> char 'B') "BCD"
Just ("CD",'B')
> runParser  (char 'A' <|> char 'B') "CDE"
Nothing

GHCi> runParser (many digit) "42abdef"
Just ("abdef",[4,2])
GHCi> runParser (some digit) "42abdef"
Just ("abdef",[4,2])

GHCi> runParser (many digit) "abdef"
Just ("abdef",[])
GHCi> runParser (some digit) "abdef"
Nothing

GHCi> runParser (optional digit) "42abdef"
Just ("2abdef",Just 4)
GHCi> runParser (optional digit) "abdef"
Just ("abdef",Nothing)
-}

-- пример рекурсивного разбора
