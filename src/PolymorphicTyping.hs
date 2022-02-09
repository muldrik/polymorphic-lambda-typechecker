module PolymorphicTyping where

import Data.Foldable
import Data.Maybe
import Control.Monad
import qualified Data.Map as Mp

infixl 4 :@
infixr 3 :->

type VarSymb = String
type TypeSymb = String 

-- Терм
data Expr = Var VarSymb
          | Expr :@ Expr
          | Lam VarSymb Type Expr
          | BigLam TypeSymb Expr
          | Expr :$ Type
  deriving (Eq,Show)

-- Тип
data Type = TVar VarSymb 
          | Type :-> Type
          | Forall TypeSymb Type
  deriving (Eq,Show)

-- Контекст
newtype Env = Env (Mp.Map VarSymb Type)
  deriving (Eq,Show)


getFreshVar :: Env -> TypeSymb
getFreshVar environment = helper environment 0 where
    helper :: Env -> Int -> TypeSymb
    helper (Env env) index = let try = "sigma" ++ show index in
         if not (try `Mp.member` env) then try else helper (Env env) (index + 1)
  

insertToEnv :: VarSymb -> Type -> Env -> Env
insertToEnv s t (Env env) = Env (Mp.insert s t env)

removeFromEnv :: VarSymb -> Env -> Env
removeFromEnv s (Env env) = Env (Mp.delete s env)

emptyEnv :: Env
emptyEnv = Env Mp.empty


inferType :: Env -> Expr -> Maybe Type
inferType (Env env) (Var x) = Mp.lookup x env

inferType environment (e1 :@ e2) = do 
  (argExpected :-> ret) <- inferType environment e1
  argActual <- inferType environment e2
  guard $ argExpected == argActual
  return ret

inferType (Env env) (Lam arg argType expr) = do
  let newEnv = Env $ Mp.insert arg argType env
  exprType <- inferType newEnv expr
  return $ argType :-> exprType 

inferType environment (expr :$ typeArg) = do
    (Forall typeArgExpected ret) <- inferType environment expr
    guard $ TVar typeArgExpected == typeArg 
    return ret

inferType environment (BigLam arg expr) = do 
    exprType <- inferType environment expr 
    return $ Forall (getFreshVar environment) exprType

