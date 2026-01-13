module Comp where

import Gates
import ANF
import AST
import Data.List (nub)

buildPhase :: ANF -> QP
buildPhase (Pos i) = [Single Z i]
buildPhase (Cst True) = [Single Z 0] -- as Z's are symmetric, this effectively flips all marks
buildPhase (Cst False) = [] -- has no effect on the actual solution
buildPhase (Xor e1 e2) = buildPhase e1 ++ buildPhase e2
buildPhase (And e1 e2) = 
  [CZ (nub $ collect e1 ++ collect e2)]
  where
    collect anf =
      case anf of
        Pos i -> [i]
        And a b -> collect a ++ collect b
        _ -> [] -- No Xors are nested. We never create false constants. True is then ignored.

phaseOracle :: Exp -> QP
phaseOracle = buildPhase . astToAnf
