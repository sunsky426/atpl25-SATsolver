import qualified Eval_Tests
import qualified MV_Tests
import qualified Tests
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain $
    testGroup
      "SATQ"
      [ Eval_Tests.tests,
        MV_Tests.tests,
        Tests.tests
      ]
