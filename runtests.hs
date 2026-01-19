import qualified GenEval_Tests
import qualified Grover_Tests
import qualified SpecEval_PropTests
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.QuickCheck

main :: IO ()
main =
  defaultMain $
    testGroup
      "SATQ"
      [ GenEval_Tests.tests,
        Grover_Tests.tests,
        SpecEval_PropTests.tests
      ]
