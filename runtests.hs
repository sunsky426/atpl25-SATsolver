import qualified Tests
import qualified GenEval_Tests
import qualified SpecEval_PropTests
-- import qualified Tests
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.QuickCheck

main :: IO ()
main =
  defaultMain $
    testGroup
      "SATQ"
      [ GenEval_Tests.tests,
        SpecEval_PropTests.tests
      ]
