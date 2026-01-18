module GenEval.Comp where

import GenEval.Gates
import GenEval.ANF
import GenEval.AST
import qualified Data.Set as S

buildPhase :: ANF -> Circuit
buildPhase (Pos i) = [Ctrl Z (S.singleton i) i]
buildPhase (Cst True) = [Ctrl Z (S.singleton 0) 0] -- flips all solutions
buildPhase (Cst False) = [] -- has no effect on the actual solutions
buildPhase (Xor e1 e2) = buildPhase e1 ++ buildPhase e2
buildPhase (And e1 e2) = 
  case collect e1 ++ collect e2 of
    [] -> [Sing I (S.singleton 0)]
    (targ:[])    -> [Ctrl Z (S.singleton targ) targ]
    (targ:ctrls) -> [Ctrl Z (S.fromList ctrls) targ]
  where
    collect anf =
      case anf of
        Pos i -> [i]
        And a b -> collect a ++ collect b
        _ -> [] -- No Xors are nested. We never create false constants. True is then ignored.

phaseOracle :: Exp -> Circuit
phaseOracle = buildPhase . astToAnf
