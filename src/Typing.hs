module Typing where
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
  deriving (Eq,Show)

-- Тип
data Type = TVar TypeSymb 
          | Type :-> Type
  deriving (Eq,Show)

-- Контекст
newtype Env = Env (Mp.Map VarSymb Type)
  deriving (Eq,Show)



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




