module ANF where

import AST

data ANF
  = Cst Bool
  | Pos Int
  | Xor ANF ANF
  | And ANF ANF
  deriving (Eq, Show)

translateAstToAnf :: Exp -> ANF
translateAstToAnf (Const b) = Cst b
translateAstToAnf (Var i) = Pos i
translateAstToAnf (NEG e) = Xor (astToAnf e) (Cst True)
translateAstToAnf (XOR e1 e2) = Xor (astToAnf e1) (astToAnf e2)
translateAstToAnf (AND e1 e2) = And (astToAnf e1) (astToAnf e2)
translateAstToAnf (OR e1 e2) =
  let e1' = astToAnf e1
      e2' = astToAnf e2
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

-- double normalization may be needed in the future (unlikely)
astToAnf :: Exp -> ANF
astToAnf = dist . normalizeAnf . translateAstToAnf
