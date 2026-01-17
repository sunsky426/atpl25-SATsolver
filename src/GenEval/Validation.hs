module GenEval.Validation where

import GenEval.AST
import GenEval.Generator
import Data.Maybe
import Prelude hiding (exp)

type Solution = [Bool]
type BadSolution = (Exp, [Bool])
data GenConfig = GC {
  seed  :: Int,
  count :: Int,
  size  :: Int
}

validate :: Exp -> [Bool] -> Bool
validate bexp ass =
  case bexp of
    AND e1 e2 -> validateBoth e1 e2 (&&)
    OR e1 e2  -> validateBoth e1 e2 (||)
    XOR e1 e2 -> validateBoth e1 e2 (/=)
    NEG e     -> not (validate e ass)
    Var index -> ass !! index
    Const bool-> bool
  where
    validateBoth e1 e2 f = f (validate e1 ass) (validate e2 ass)

validatePipeline :: (Exp -> Solution) -> GenConfig -> Maybe [BadSolution]
validatePipeline pipeline config =
  let 
      -- generate expressions for validation testing, based on injected config.
      exps = genMany (seed config) (count config) (size config)

      -- evaluate generated expressions for the given pipeline/evaluation module.
      evaluated = map pipeline exps

      -- a check function for (in)validating solutions to expressions
      check res exp' =
        case validate exp' res of
          True  -> Nothing
          False -> Just (exp',res)

      -- actually checking each pipeline/evaluator result
      checked = mapMaybe (\(res,exp) -> check res exp) $ zip evaluated exps

      -- returning all counterexamples. If the pipeline/evaluator is correct, 
      -- `Nothing` is always returned
   in case checked of
        [] -> Nothing
        counterExamples -> Just counterExamples
