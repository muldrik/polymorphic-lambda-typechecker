import Test.Tasty.HUnit
import Test.Tasty
import System.Environment (setEnv)
import qualified Data.Map as Mp
import PolymorphicTyping
import Parser (parserResult)
import TypingIO
import Control.Monad.Except(runExcept)

main :: IO ()
main = do
    setEnv "TASTY_COLOR" "always"
    defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [simplyTypedInferenceTests, polymorphicInferenceTests, parserTests]

bigEnv :: Env
bigEnv = Env $ Mp.fromList [("zzz" ++ show i, TVar $ "alpha" ++ show i) | i <- bigList] where
    bigList :: [Int]
    bigList = [1..40000]

simplyTypedInferenceTests :: TestTree
simplyTypedInferenceTests = testGroup "Type infernce: simply typed calculus" 
    [
    testGroup "single variable"
        [
            testCase "Type given in a context" $ let
                v = "x"
                expectedType = TVar "a"
                env = (v, expectedType) ## emptyEnv 
                    in runExcept (inferType env (Var v)) @?= Right expectedType,
            testCase "Not found in context" $ let
                v = "x"
                env = emptyEnv
                    in runExcept (inferType env (Var v)) @?= Left (FreeVarNotInContext v),
            testCase "Large environment" $ let
                v = "x"
                expectedType = TVar "a"
                env = (v, expectedType) ## bigEnv
                    in runExcept (inferType env (Var v)) @?= Right expectedType,
            testCase "Lambda type in a context" $ runExcept (inferType (("x", TVar "a" :-> TVar "b") ## emptyEnv) (Var "x")) @?= Right (TVar "a" :-> TVar "b")
        ],
    testGroup "simple lambda" 
        [
            testCase "a -> a" $ runExcept (inferType emptyEnv (Lam "x" (TVar "a") (Var "x"))) @?= Right (TVar "a" :-> TVar "a"),
            testCase "(a -> b) -> c" $ runExcept (inferType (("y", TVar "c") ## emptyEnv) (Lam "x" (TVar "a" :-> TVar "b") (Var "y"))) @?= Right ((TVar "a" :-> TVar "b") :-> TVar "c"),
            testCase "a -> b -> a" $ runExcept (inferType emptyEnv (Lam "x" (TVar "a") (Lam "y" (TVar "b") (Var "x")))) @?= Right (TVar "a" :-> TVar "b" :-> TVar "a")
        ],
    testGroup "application"
        [
            testCase "var to lambda" $ runExcept (inferType (("y", TVar "a") ## emptyEnv) (Lam "x" (TVar "a") (Var "x") :@ Var "y")) @?= Right (TVar "a"),
            testCase "lambda to lambda" $ runExcept (inferType (("y", TVar "b") ## emptyEnv) (Lam "x" (TVar "a" :-> TVar "b") (Var "x") :@ Lam "x" (TVar "a") (Var "y"))) @?= Right (TVar "a" :-> TVar "b"),
            testCase "valid ap to a free variable" $ runExcept (inferType (("x", TVar "alpha" :-> TVar "beta") ## ("y", TVar "alpha") ## emptyEnv) (Var "x" :@ Var "y")) @?= Right (TVar "beta"), 
            testCase "shadowing a free variable" $ runExcept (inferType (("x", (TVar "a" :-> TVar "a") :-> TVar "b") ## emptyEnv) (Var "x" :@ Lam "x" (TVar "a") (Var "x"))) @?= Right (TVar "b")
        ],
    testGroup "Error handling" []
    ]

inferAndAlphaCompare :: Env -> Expr -> Type -> Bool
inferAndAlphaCompare env expr expected = case comp of
    Right True -> True 
    _ -> False
    where 
        comp = do
            actual <- runExcept (inferType env expr)
            return $ alphaEq actual expected

polymorphicInferenceTests :: TestTree
polymorphicInferenceTests = testGroup "System F type inference"
    [
        testGroup "BigLambda abstaction" 
        [
            testCase "@a . \\x : a . x" $ assertBool "" $ inferAndAlphaCompare emptyEnv (BigLam "sigma" (Lam "x" (TVar "sigma") (Var "x"))) (Forall "sigma" (TVar "sigma" :-> TVar "sigma"))
--            testCase "@a . @b . \\x : (a) y : (b) . "
        ],
        testGroup "Type substitution"
        [
            testCase "simple sub" $ assertBool "" $ inferAndAlphaCompare emptyEnv (BigLam "a" $ Lam "x" (TVar "a") (Var "x")) (Forall "a" (TVar "a" :-> TVar "a")),
            testCase "global type shadowing: #a b. \\x:a y:b . x) {b}" $ assertBool "" $ inferAndAlphaCompare emptyEnv ((BigLam "a" $ BigLam "b" $ Lam "x" (TVar "a") $ Lam "y" (TVar "b") (Var "x")) :$ TVar "b") (Forall "c" (TVar "b" :-> TVar "c" :-> TVar "b")),
            testCase "free type shadowing" $ assertBool "" $ inferAndAlphaCompare emptyEnv ((BigLam "a" $ BigLam "b" $ Lam "x" (TVar "a") $ Lam "y" (TVar "b") $ Lam "z" (Forall "d" $ TVar "d" :-> TVar "b" :-> TVar "e") (Var "z")) :$ (TVar "d" :-> TVar "b")) (Forall "t1" $ (TVar "d" :-> TVar "b") :-> TVar "t1" :-> Forall "t2" (TVar "t2" :-> TVar "t1" :-> TVar "e") :-> Forall "t2" (TVar "t2" :-> TVar "t1" :-> TVar "e")) 
        ]
    ]

parserTests :: TestTree
parserTests = testGroup "Parser tests"
    [
        testGroup "Single variable"
            [
                testCase "lowercase" $ parserResult expression "variable" @?= Just (Var "variable"),
                testCase "snace case" $ parserResult expression "my_var" @?= Just (Var "my_var"),
                testCase "Capitalized" $ parserResult expression "Variable" @?= Just (Var "Variable"),
                testCase "Invalid variable: beginning with a number" $ parserResult expression "1var" @?= Nothing,
                testCase "Invalid variable: hyphen" $ parserResult expression "bad-var" @?= Nothing,
                testCase "Extra parentheses" $ parserResult expression "(((var)))" @?= Just (Var "var"),
                testCase "Extra spaces" $ parserResult expression "   (   var  )  " @?= Just (Var "var")
            ],
        testGroup "Single lambda"
            [
                testCase "standard notaion: one argument" $ parserResult expression "\\x : alpha . x" @?= Just (Lam "x" (TVar "alpha") (Var "x")),
                testCase "standard notaion: one argument" $ parserResult expression "\\x : a . \\y : b . x y" @?= Just (Lam "x" (TVar "a") (Lam "y" (TVar "b") (Var "x" :@ Var "y"))),
                testCase "extended notation: multiple arguments" $ parserResult expression "\\x : a y : b . x y" @?= Just (Lam "x" (TVar "a") (Lam "y" (TVar "b") (Var "x" :@ Var "y"))),
                testCase "weird spacing" $ parserResult expression "\\x:a       y :  b.x y  " @?= Just (Lam "x" (TVar "a") (Lam "y" (TVar "b") (Var "x" :@ Var "y"))),
                testCase "weird brackets" $ parserResult expression "((\\x : (a) . ((x))))" @?= Just (Lam "x" (TVar "a") (Var "x")),
                testCase "application to a lambda" $ parserResult expression "(\\x : a . x) y" @?= Just (Lam "x" (TVar "a") (Var "x") :@ Var "y")
            ],
        testGroup "Simple application"
            [
                testCase "2 variables" $ parserResult expression "a b" @?= Just (Var "a" :@ Var "b"),
                let manyVariables = [[c1, c2, c3] | c1 <- ['a'..'z'], c2 <- ['a'..'z'], c3 <- ['a'..'z']]
                    input = unwords manyVariables
                    varList = Var <$> manyVariables in
                    testCase "many variables" $ parserResult expression input @?= Just (foldl1 (:@) varList)
            ],
        testGroup "Types"
            [
                testCase "non-arrow" $ parserResult typeParser "a" @?= Just (TVar "a"),
                testCase "extra brackets" $ parserResult typeParser "(((a)))" @?= Just (TVar "a"),
                testCase "extra spaces" $ parserResult typeParser "   (   a) " @?= Just (TVar "a"),
                testCase "simple arrow type" $ parserResult typeParser "a -> b" @?= Just (TVar "a" :-> TVar "b"),
                testCase "complex arrow type" $ parserResult typeParser "((c -> b) -> (a -> c) -> a -> b)" @?= Just ((TVar "c" :-> TVar "b") :-> (TVar "a" :-> TVar "c") :-> TVar "a":-> TVar "b"),
                testCase "simple forall" $ parserResult typeParser "@a . a" @?= Just (Forall "a" (TVar "a")),
                testCase "extended notation: multiple forall arguments" $ parserResult typeParser "@a b c . (a -> b) -> c" @?= Just (Forall "a" (Forall "b" (Forall "c" ((TVar "a" :-> TVar "b") :-> TVar "c"))))
            ],
        testGroup "Big lambda"
            [
                testCase "standard notation: one argument" $ parserResult expression "#a . x" @?= Just (BigLam "a" (Var "x")),
                testCase "standard notation: one argument" $ parserResult expression "#a . # b. x" @?= Just (BigLam "a" (BigLam "b" (Var "x"))),
                testCase "extended notation: multiple arguments" $ parserResult expression "#a b c . x" @?= Just (BigLam "a" (BigLam "b" (BigLam "c" (Var "x")))),
                testCase "weird spacing" $ parserResult expression "   #   a . x" @?= Just (BigLam "a" (Var "x")),
                testCase "weird brackets" $ parserResult expression "(#a. ( (x )))" @?= Just (BigLam "a" (Var "x")),
                testCase "application to a lambda" $ parserResult expression "(#a . \\x : a. x) {b}" @?= Just (BigLam "a" (Lam "x" (TVar "a") (Var "x")) :$ TVar "b")
            ],
        testGroup "Type application"
            [
                testCase "simple application" $ parserResult expression "x {a -> b} {d}" @?= Just ((Var "x" :$ (TVar "a" :-> TVar "b")) :$ TVar "d")
            ]
    ]


