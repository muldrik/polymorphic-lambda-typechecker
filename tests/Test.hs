import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import Test.Tasty
import Test.Tasty.Providers.ConsoleFormat (ConsoleFormat(color))
import System.Environment (setEnv)
import Dummy(dummy)

main = do
    setEnv "TASTY_COLOR" "always"
    defaultMain unitTests


unitTests :: TestTree
unitTests = testGroup "Unit tests" 
    [
    testCase "1+1=2" $ 1 + 1 @?= 2,
    testCase "Call a function from project module" $ dummy 10 @?= 20
    ]


--quickTests :: TestTree 
--quickTests = tes
