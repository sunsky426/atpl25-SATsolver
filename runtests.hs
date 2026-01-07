import qualified Eval_Tests
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain $
    testGroup
      "SATQ"
      [ Eval_Tests.tests
      ]