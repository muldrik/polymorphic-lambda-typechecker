{-# LANGUAGE TypeInType #-}
module PolymorphicTyping where

import qualified Data.Map as Mp
import Data.List(intercalate)
import Control.Monad.Except
import qualified Data.Set as St

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
  deriving (Eq, Show)

-- Тип
data Type = TVar VarSymb 
          | Type :-> Type
          | Forall TypeSymb Type
  deriving (Eq)


instance Show Type where
  showsPrec _ (TVar t) = showString t
  showsPrec p (t1 :-> t2) = showParen (p > 3) $
    showsPrec 4 t1 .
    showString " -> " .
    showsPrec 3 t2
  showsPrec p (Forall t1 t2) = showParen (p > 0) $
    showChar '@' .
    showString t1 .
    showString ". " .
    showsPrec p t2
  

-- Контекст
newtype Env = Env (Mp.Map VarSymb Type)
  deriving (Eq)


instance Show Env where
  show (Env env) = let kek (var, t) = showString var . showString ": " $ show t in intercalate ", " $ map kek (Mp.toList env)



-- Choose a type name that doesn't appear in appear anywhere in the environment
{-
getFreshVar :: Env -> TypeSymb
getFreshVar (Env env) = helper (Mp.elems env) 0 where
    helper :: [Type] -> Int -> TypeSymb
    helper types index = let 
      attempt = "sigma" ++ show index
      appearsAnywhere = any (\t -> attempt `St.member` freeTypeVars t) types
        in if appearsAnywhere then helper types (index + 1) else attempt
        -}

--Methods to use when exporting the module

infixr 4 ##
(##) :: (VarSymb, Type) -> Env -> Env
(##) = insertToEnv

envFromList :: [(VarSymb, Type)] -> Env
envFromList = Env . Mp.fromList

envToList :: Env -> [(VarSymb, Type)]
envToList (Env env) = Mp.toList env

insertToEnv :: (VarSymb, Type) -> Env -> Env
insertToEnv (s, t) (Env env) = Env (Mp.insert s t env)

removeFromEnv :: VarSymb -> Env -> Env
removeFromEnv s (Env env) = Env (Mp.delete s env)

emptyEnv :: Env
emptyEnv = Env Mp.empty


data InferenceError = FreeVarNotInContext VarSymb |
                      MismatchedApplicationTypes Expr Type Expr Type |
                      NonForallTypeApplication Expr Type |
                      NonArrowApplication Expr Type Expr
                      deriving Eq


instance Show InferenceError where
  show (FreeVarNotInContext symb) = "Free variable " ++ symb ++ " not found in context"
  show (MismatchedApplicationTypes e1 t1 e2 t2) = "Mismatched application types: left side is an expression " ++ show e1 ++ " of type " ++ show t1 ++ "; right side is an expression " ++ show e2 ++ "of type " ++ show t2
  show (NonForallTypeApplication leftExpr rightType) = "Cannot apply type " ++ show rightType ++ " to a non-BigLambda expression " ++ show leftExpr
  show (NonArrowApplication e1 t1 e2) = "Cannot apply expression " ++ show e2 ++ " to a non-arrow expression " ++ show e1 ++ " of type " ++ show t1


freeTVarsInType :: Type -> St.Set TypeSymb
freeTVarsInType (TVar x) = St.singleton x
freeTVarsInType (t1 :-> t2) = freeTVarsInType t1 `St.union` freeTVarsInType t2
freeTVarsInType (Forall a t) = a `St.delete` freeTVarsInType t

freeTVarsInExpr :: Expr -> St.Set TypeSymb
freeTVarsInExpr (Var _) = St.empty
freeTVarsInExpr (e1 :@ e2) = freeTVarsInExpr e1 `St.union` freeTVarsInExpr e2
freeTVarsInExpr (e :$ t) = freeTVarsInType t `St.union` freeTVarsInExpr e 
freeTVarsInExpr (Lam _ t e) = freeTVarsInType t `St.union` freeTVarsInExpr e
freeTVarsInExpr (BigLam t e) = t `St.delete` freeTVarsInExpr e 


infNameList :: [String]
infNameList = ('t' :) . show <$> ([1..] :: [Integer])

chooseFreshName :: St.Set TypeSymb -> TypeSymb
chooseFreshName set = helper set infNameList where
  helper s (t : xt) = if t `St.member` s then helper s xt else t
  helper _ _ = undefined




--Substitube target symbol to type 1 inside of type 2
substSymb :: TypeSymb -> Type -> Type -> Type
substSymb oldSym new (TVar t) = if t == oldSym then new else TVar t
substSymb oldSym new (t1 :-> t2) = substSymb oldSym new t1 :-> substSymb oldSym new t2
substSymb oldSym new (Forall s t) | s == oldSym = Forall s t
                                              | s `St.notMember` freeTVarsInType new = Forall s (substSymb oldSym new t)
                                              | otherwise = Forall freshForallName $ (originalSubst . forallSymbSubst) t where
                                                  newFreeVars = freeTVarsInType new            
                                                  freshForallName = chooseFreshName newFreeVars
                                                  forallSymbSubst = substSymb s (TVar freshForallName)
                                                  originalSubst = substSymb oldSym new

-- | Substitute type symbol for given type in a given expression. 
-- Can rename type arguments in BigLambdas if they shadow newly substituted free type variables
substTypeSymbInExpr :: TypeSymb -> Type -> Expr -> Expr
substTypeSymbInExpr oldSym new (Lam argVar argType expr) = Lam argVar (substSymb oldSym new argType) (substTypeSymbInExpr oldSym new expr)
substTypeSymbInExpr oldSym new e@(BigLam arg expr) | arg == oldSym = e
                                                   | arg `St.member` newFreeVars = BigLam newArgName resultingExpression
                                                   | otherwise = BigLam arg $ substTypeSymbInExpr oldSym new expr
                                                      where 
                                                      newFreeVars = freeTVarsInType new
                                                      oldFreeVars = freeTVarsInExpr expr
                                                      allFreeVars = newFreeVars `St.union` oldFreeVars
                                                      newArgName = chooseFreshName allFreeVars
                                                      fixedExpression = substTypeSymbInExpr arg (TVar newArgName) expr
                                                      resultingExpression = substTypeSymbInExpr oldSym new fixedExpression
substTypeSymbInExpr oldSym new (e1 :@ e2) = substTypeSymbInExpr oldSym new e1 :@ substTypeSymbInExpr oldSym new e2
substTypeSymbInExpr _ _ (Var x) = Var x
substTypeSymbInExpr oldSym new (e :$ t) = substTypeSymbInExpr oldSym new e :$ substSymb oldSym new t





inferType :: Env -> Expr -> Except InferenceError Type
inferType (Env env) (Var x) = case Mp.lookup x env of
  Nothing -> throwError $ FreeVarNotInContext x  
  Just actual -> return actual
  

inferType environment (e1 :@ e2) = do
  leftType <- inferType environment e1
  case leftType of
    (argExpected :-> ret) -> do 
      argActual <- inferType environment e2
      if argExpected == argActual then return ret 
        else throwError $ MismatchedApplicationTypes e1 (argExpected :-> ret) e2 argActual 
    nonArrow -> throwError $ NonArrowApplication e1 nonArrow e2

inferType (Env env) (Lam arg argType expr) = do
  let newEnv = Env $ Mp.insert arg argType env
  exprType <- inferType newEnv expr
  return $ argType :-> exprType 

inferType environment (expr :$ typeArg) = do
  leftType <- inferType environment expr
  case leftType of
    (Forall t e) -> return $ substSymb t typeArg e
    _ -> throwError $ NonForallTypeApplication expr typeArg

inferType environment (BigLam arg expr) = do 
    exprType <- inferType environment expr  
    return $ Forall arg exprType



alphaEq :: Type -> Type -> Bool
alphaEq (TVar a) (TVar b) = a == b
alphaEq (t1 :-> t2) (t1' :-> t2') = alphaEq t1 t1' && alphaEq t2 t2'
alphaEq (Forall arg1 t1) (Forall arg2 t2) | arg1 == arg2 = alphaEq t1 t2
                                          | otherwise = alphaEq t1 (substSymb arg2 (TVar arg1) t2)
alphaEq _ _ = False