module GenEval.ANF where

import GenEval.AST
import GenEval.Gates
--import Data.List (sort,nub)
import qualified Data.Set as S
--import qualified Data.Map.Strict as M

type MonoANF = [[Int]]

data ANF
  = Cst Bool
  | Pos Int
  | Xor ANF ANF
  | And ANF ANF
  deriving (Eq, Show)

translateAstToAnf :: Exp -> ANF
translateAstToAnf (Const b) = Cst b
translateAstToAnf (Var i) = Pos i
translateAstToAnf (NEG e) = Xor (translateAstToAnf e) (Cst True)
translateAstToAnf (XOR e1 e2) = Xor (translateAstToAnf e1) (translateAstToAnf e2)
translateAstToAnf (AND e1 e2) = And (translateAstToAnf e1) (translateAstToAnf e2)
translateAstToAnf (OR e1 e2) =
  let e1' = translateAstToAnf e1
      e2' = translateAstToAnf e2
   in Xor (Xor e1' e2') (And e1' e2') -- equivalent to elimORwXOR

-- distribute And across Xor.
dist :: ANF -> ANF
dist anf =
  case anf of
    And e1 e2 -> distAnd (dist e1) (dist e2)
    Xor e1 e2 -> Xor (dist e1) (dist e2)
    _ -> anf
  where
    distAnd e1 e2 =
      case (e1,e2) of
        (Xor a b,c) -> Xor (distAnd a c) (distAnd b c)
        (a,Xor b c) -> Xor (distAnd a b) (distAnd a c)
        _ -> And e1 e2

normalizeRepeated :: ANF -> ANF
normalizeRepeated anf =
  let anf' = normalizeAnf anf
   in if anf' == anf then anf' else normalizeRepeated anf'

-- reduce ANF
normalizeAnf :: ANF -> ANF
normalizeAnf anf =
  case anf of
    -- AND reductions
    And (Cst False) _ -> Cst False
    And _ (Cst False) -> Cst False
    And (Cst True) a  -> normalizeAnf a
    And a (Cst True)  -> normalizeAnf a

    -- XOR reductions
    Xor a (Cst False) -> normalizeAnf a
    Xor (Cst False) a -> normalizeAnf a

    -- normalize whole expression tree
    And a b -> And (normalizeAnf a) (normalizeAnf b)
    Xor a b -> Xor (normalizeAnf a) (normalizeAnf b)
    _ -> anf


astToAnf :: Exp -> ANF
astToAnf = normalizeRepeated . dist . translateAstToAnf

-- a simple type that dictates, if the total ANF needs to be false or true
type TCirc = (Bool,Circuit)

phaseOracle :: Exp -> TCirc
phaseOracle e =
  let anf = astToAnf e
   in gates anf True
  where
    gates (Pos i) b = (b,[Sing Z (S.singleton i)])
    gates (Cst True) b = (not b,[])
    gates (Cst False) b = (b,[])
    gates (Xor e1 e2) b =
      let (b1,e1') = gates e1 b
          (b2,e2') = gates e2 b1
       in (b2,e1' ++ e2')
    gates anf@(And _ _) b =
      let atom = collect anf
       in (b,[Ctrl Z (S.fromList $ init atom) (last atom)])
      where
        collect anf' =
          case anf' of
            Pos i -> [i]
            And as bs -> collect as ++ collect bs
            _ -> []
