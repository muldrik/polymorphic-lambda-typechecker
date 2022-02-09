import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import Test.Tasty
import Test.Tasty.Providers.ConsoleFormat (ConsoleFormat(color))
import System.Environment (setEnv)
import qualified Data.Map as Mp
import PolymorphicTyping

main = do
    setEnv "TASTY_COLOR" "always"
    defaultMain tests

tests :: TestTree 
tests = testGroup "Tests" [inferVarTests]

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



typingProps = undefined



--quickTests :: TestTree 
--quickTests = tes
