module PolymorphicTyping where

import Data.Foldable
import Data.Maybe
import Control.Monad
import qualified Data.Map as Mp

infixl 4 :@
infixr 3 :->

type Symb = String 

-- Терм
data Expr = Var Symb
          | Expr :@ Expr
          | Lam Symb Type Expr
  deriving (Eq,Show)

-- Тип
data Type = TVar Symb 
          | Type :-> Type
  deriving (Eq,Show)

-- Контекст
newtype Env = Env (Mp.Map Symb Type)
  deriving (Eq,Show)