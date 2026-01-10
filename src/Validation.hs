module Validation where

import AST
import Generator
import Data.Maybe
import Prelude hiding (exp)

type Solution = Maybe [Bool]
data GenConfig = GenConfig {
  seed  :: Int,
  count :: Int,
  size  :: Int
}
data CounterExample = CounterExample {
  exp :: Exp,
  proposition :: Solution,
  solution :: Solution
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


-- only needed for validating that an expression is not satisfiable.
bruteSATSolver :: Exp -> Solution
bruteSATSolver _ = Nothing -- TODO: add actual SAT solving logic


validatePipeline :: (Exp -> Solution) -> GenConfig -> Maybe [CounterExample]
validatePipeline pipeline config =
  let 
      -- generate expressions for validation testing, based on injected config.
      exps = genMany (seed config) (count config) (size config)

      -- evaluate generated expressions for the given pipeline/evaluation module.
      evaluated = map pipeline exps

      -- a check function for (in)validating solutions to expressions
      check res exp' = 
        case res of
          Nothing -> 
            case bruteSATSolver exp' of
              Nothing -> Nothing
              Just res' -> 
                Just $ CounterExample 
                  { exp = exp', 
                    proposition = Nothing, 
                    solution = Just res' 
                  }
          Just res' -> 
            if validate exp' res'
              then Nothing 
              else Just $ CounterExample { exp = exp', proposition = Just res', solution = Nothing}

      -- actually checking each pipeline/evaluator result
      checked = mapMaybe (\(res,exp) -> check res exp) $ zip evaluated exps

      -- returning all counterexamples. If the pipeline/evaluator is correct, 
      -- `Nothing` is always returned
   in case checked of
        [] -> Nothing
        counterExamples -> Just counterExamples
