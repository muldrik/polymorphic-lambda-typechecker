module Main where
import System.Environment (getArgs)
import TypingIO
import PolymorphicTyping
import Parser
import Control.Monad.Except

data MainError = ArgNumError Int |
                 EnvParsingError |
                 ExprParsingError |
                 TypeParsingError


instance Show MainError where
  show (ArgNumError n) = "Expected 3 program arguments, instead " ++ show n ++ " were given"
  show EnvParsingError = "Failed to parse the environment argument"
  show ExprParsingError = "Failed to parse the expression argument"
  show TypeParsingError = "Failed to parse the expected type argument"

validateArgNum :: [String] -> Except MainError ()
validateArgNum args = when (length args /= 3) $ throwError $ ArgNumError $ length args

parseEnv :: String -> Except MainError Env
parseEnv s = do
  case parserResult environmentParser s of
    Nothing -> throwError EnvParsingError
    Just env -> return env

parseExpr :: String -> Except MainError Expr
parseExpr s = do
  case parserResult expression s of
    Nothing -> throwError ExprParsingError
    Just expr -> return expr

parseType :: String -> Except MainError Type
parseType s = do
  case parserResult typeParser s of
    Nothing -> throwError TypeParsingError
    Just t -> return t

parseArgs :: [String] -> Except MainError (Env, Expr, Type)
parseArgs args = do
  validateArgNum args
  env <- parseEnv (head args)
  expr <- parseExpr (args !! 1)
  expectedType <- parseType (args !! 2)
  return (env, expr, expectedType)


main :: IO ()
main = do
  args <- getArgs 
  case runExcept $ parseArgs args of
    Left e -> print $ "Parsing error: " ++ show e
    Right (env, expr, expectedType) -> case runExcept $ inferType env expr of
      Left e -> print $ "Type inference error:" ++ show e
      Right inferredType -> if alphaEq expectedType inferredType then print "Typecheck successful"
        else print $ "Typechecking failed: actual expression type is " ++ show inferredType ++ " which is not alpha-equivalent to expected type " ++ show expectedType
