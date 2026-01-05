module Comp where

import Gates
import ANF
import AST
import Data.List (nub)

buildPhase :: ANF -> QP
buildPhase (Pos i) = [Single Z i]
buildPhase (Cst _) = []
buildPhase (Xor e1 e2) = buildPhase e1 ++ buildPhase e2
buildPhase (And e1 e2) = 
  [CZ (nub $ collect e1 ++ collect e2)]
  where
    collect anf =
      case anf of
        Pos i -> [i]
        And a b -> collect a ++ collect b
        _ -> [] --error $ "ANF is invalid; " ++ show anf ++ " is not a monomial."

phaseOracle :: Exp -> QP
phaseOracle = buildPhase . astToAnf
