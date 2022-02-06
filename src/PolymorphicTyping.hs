module PolymorphicTyping where

import Data.Foldable
import Data.Maybe
import Control.Monad
import qualified Data.Map as Mp
import System.Environment (getEnvironment)
import GHC.ResponseFile (expandResponse)

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
    helper (Env env) index = let try = "sigma" ++ show index in
         if not (try `Mp.member` env) then try else helper (Env env) (index + 1)


deriveType :: Env -> Expr -> Maybe Type
deriveType (Env env) (Var x) = Mp.lookup x env

deriveType environment (e1 :@ e2) = do 
  (argExpected :-> ret) <- deriveType environment e1
  argActual <- deriveType environment e2
  guard $ argExpected == argActual
  return ret

deriveType (Env env) (Lam arg argType expr) = do
  let newEnv = Env $ Mp.insert arg argType env
  exprType <- deriveType newEnv expr
  return $ argType :-> exprType 

deriveType environment (expr :$ typeArg) = do
    (Forall typeArgExpected ret) <- deriveType environment expr
    guard $ TVar typeArgExpected == typeArg 
    return ret

deriveType environment (BigLam arg expr) = do 
    exprType <- deriveType environment expr 
    return $ Forall (getFreshVar environment) exprType

