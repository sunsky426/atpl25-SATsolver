module Transform where
import AST

orToXor :: Exp -> Exp
orToXor (OR a b) = XOR (XOR a b) (AND a b)
orToXor e = e

andReduc :: Exp -> Exp
andReduc e@(AND e1 e2) = 
    case (e1, e2) of 
        (Var s1, Var s2) | s1 == s2  -> Var s1 
        (NEG (Var s1), Var s2) | s1 == s2 -> Const False
        (Var s1, NEG (Var s2)) | s1 == s2 -> Const False
        (Const True, e2') -> e2'
        (e1', Const True) -> e1' 
        (Const False, _) -> Const False
        (_, Const False) -> Const False
        _ -> e
andReduc e = e

orReduc :: Exp -> Exp
orReduc e@(OR e1 e2) = 
    case (e1, e2) of 
        (Var s1, Var s2) | s1 == s2  -> Var s1 
        (NEG (Var s1), Var s2) | s1 == s2 -> Const True
        (Var s1, NEG (Var s2)) | s1 == s2 -> Const True
        (Const False, e2') -> e2'
        (e1', Const False) -> e1'
        (Const True, _) -> Const True
        (_, Const True) -> Const True
        _ -> e
orReduc e = e

xorReduc :: Exp -> Exp
xorReduc e@(XOR e1 e2) = 
    case (e1, e2) of 
        (Var s1, Var s2) | s1 == s2 -> Const False 
        (NEG (Var s1), Var s2) | s1 == s2 -> Const True
        (Var s1, NEG (Var s2)) | s1 == s2 -> Const True
        (Const False, e2') -> e2'
        (e1', Const False) -> e1'
        (Const True, e2') -> NEG e2'
        (e1', Const True) -> NEG e1'
        _ -> e
xorReduc e = e

negReduc :: Exp -> Exp 
negReduc (NEG (NEG e)) = negReduc e
negReduc (NEG (Const True)) = Const False
negReduc (NEG (Const False)) = Const True
negReduc e = e

transform :: (Exp -> Exp) -> Exp -> Exp
transform f e = 
    case e of 
        (OR e1 e2) -> f $ OR (transform f e1) (transform f e2)
        (AND e1 e2) -> f $ AND (transform f e1) (transform f e2)
        (XOR e1 e2) -> f $ XOR (transform f e1) (transform f e2)
        _ -> f e