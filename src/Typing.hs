module Typing where

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
newtype Env = Env [(Symb,Type)]
  deriving (Eq,Show)


checkType :: Env -> Expr -> Type -> Bool
checkType env (Var x) (TVar t) = undefined 

kek x y = x + y

