module GenEval.Comp where

import GenEval.Gates
import GenEval.ANF
import GenEval.AST
import qualified Data.Set as S
import Data.List (nub)

buildPhase :: ANF -> Circuit
buildPhase (Pos i) = [Ctrl Z (S.singleton i) i]
buildPhase (Cst True) = [Ctrl Z (S.singleton 0) 0] -- flips all solutions
buildPhase (Cst False) = [] -- has no effect on the actual solutions
buildPhase (Xor e1 e2) = buildPhase e1 ++ buildPhase e2
buildPhase (And e1 e2) = 
  let (targ:ctrls) = collect e1 ++ collect e2
   in [Ctrl Z (S.fromList ctrls) targ]
      where
        collect anf =
          case anf of
            Pos i -> [i]
            And a b -> collect a ++ collect b
            _ -> [] -- No Xors are nested. We never create false constants. True is then ignored.

phaseOracle :: Exp -> Circuit
phaseOracle = buildPhase . astToAnf
