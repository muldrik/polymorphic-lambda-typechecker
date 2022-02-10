import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import Test.Tasty
import Test.Tasty.Providers.ConsoleFormat (ConsoleFormat(color))
import System.Environment (setEnv)
import qualified Data.Map as Mp
import PolymorphicTyping
import Parser (parserResult)
import TypingIO

main = do
    setEnv "TASTY_COLOR" "always"
    defaultMain tests

tests :: TestTree 
tests = testGroup "Tests" [inferVarTests, parserTests]

bigEnv :: Env
bigEnv = Env $ Mp.fromList [("zzz" ++ show i, TVar $ "alpha" ++ show i) | i <- [1..40000]]

inferVarTests = testGroup "Infer a single variable" 
    [
        testCase "Type given in a context" $ let
            var = "x"
            expectedType = TVar "a"
            env = Env $ Mp.fromList [(var, expectedType)] 
                in  inferType env (Var var) @?= Just expectedType,
        testCase "Empty context" $ let
            var = "x"
            expectedType = TVar "a"
            env = Env $ Mp.empty 
                in inferType env (Var var) @?= Nothing,
        testCase "Large environment" $ let
            var = "x"
            expectedType = TVar "a"
            env = insertToEnv var expectedType bigEnv
                in inferType env (Var var) @?= Just expectedType
    ]


parserTests = testGroup "Parser tests"
    [
        testGroup "singleVariable"
            [
                testCase "lowercase" $ parserResult expression "variable" @?= Just (Var "variable"),
                testCase "snace case" $ parserResult expression "my_var" @?= Just (Var "my_var"),
                testCase "Capitalized" $ parserResult expression "Variable" @?= Just (Var "Variable"), 
                testCase "Invalid variable: beginning with a number" $ parserResult expression "1var" @?= Nothing,
                testCase "Invalid variable: hyphen" $ parserResult expression "bad-var" @?= Nothing,
                testCase "Extra parentheses" $ parserResult expression "(((var)))" @?= Just (Var "var"),
                testCase "Extra spaces" $ parserResult expression "   (   var  )  " @?= Just (Var "var")
            ],
        testGroup "singleLambda"
            [
                testCase "standard notaion" $ parserResult expression "\\x : alpha . x" @?= Just (Lam "x" (TVar "alpha") (Var "x")),
                testCase "multiple arguments" $ parserResult expression "\\x : a y : b . x y" @?= Just (Lam "x" (TVar "a") (Lam "y" (TVar "b") (Var "x" :@ Var "y"))),
                testCase "weird spacing" $ parserResult expression "\\x:a       y :  b.x y  " @?= Just (Lam "x" (TVar "a") (Lam "y" (TVar "b") (Var "x" :@ Var "y"))),
                testCase "weird brackets" $ parserResult expression "((\\x : (a) . ((x))))" @?= Just (Lam "x" (TVar "a") (Var "x"))
            ],
        testGroup "simple ap"
            [
                testCase "2 variables" $ parserResult expression "a b" @?= Just (Var "a" :@ Var "b"),
                let manyVariables = [[c1, c2, c3] | c1 <- ['a'..'z'], c2 <- ['a'..'z'], c3 <- ['a'..'z']]
                    input = unwords manyVariables --Concatenate with spaces
                    varList = Var <$> manyVariables in
                    testCase "many variables" $ parserResult expression input @?= Just (foldl1 (:@) varList)
            ],
        testGroup "types"
            [
                testCase "non-arrow" $ parserResult typeParser "a" @?= Just (TVar "a"),
                testCase "extra brackets" $ parserResult typeParser "(((a)))" @?= Just (TVar "a"),
                testCase "extra spaces" $ parserResult typeParser "   (   a) " @?= Just (TVar "a"),
                testCase "simple arrow type" $ parserResult typeParser "a -> b" @?= Just (TVar "a" :-> TVar "b"),
                testCase "complex arrow type" $ parserResult typeParser "((c -> b) -> (a -> c) -> a -> b)" @?= Just ((TVar "c" :-> TVar "b") :-> (TVar "a" :-> TVar "c") :-> TVar "a":-> TVar "b")
            ]
    ]

typingProps = undefined



--quickTests :: TestTree 
--quickTests = tes
