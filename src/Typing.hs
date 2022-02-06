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




