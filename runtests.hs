import qualified GenEval_Tests
-- import qualified Tests
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.QuickCheck

main :: IO ()
main =
  defaultMain $
    testGroup
      "SATQ"
      [ GenEval_Tests.tests
      ]