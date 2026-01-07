module Validation where

import AST

validate :: Exp -> [Bool] -> Bool
validate bexp ass =
  case bexp of
    AND e1 e2 -> validateBoth e1 e2 (&&)
    OR e1 e2  -> validateBoth e1 e2 (||)
    XOR e1 e2 -> validateBoth e1 e2 (/=)
    NEG e     -> not (validate e ass)
    Var index -> ass !! index
  where
    validateBoth e1 e2 f = f (validate e1 ass) (validate e2 ass)
