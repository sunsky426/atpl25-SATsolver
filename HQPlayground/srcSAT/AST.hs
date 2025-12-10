module AST where

data Exp = 
    Var Int
  | AND Exp Exp 
  | OR Exp Exp
  | XOR Exp Exp 
  | NEG Exp
  deriving (Show, Eq)

elimORwXOR :: Exp -> Exp
elimORwXOR (OR a b) = XOR (XOR a b) (AND a b)
elimORwXOR (AND a b) = AND (elimORwXOR a) (elimORwXOR b)
elimORwXOR (XOR a b) = XOR (elimORwXOR a) (elimORwXOR b)
elimORwXOR e = e

elimORwMorgan :: Exp -> Exp
elimORwMorgan (OR a b) = NEG (AND (NEG a) (NEG b))
elimORwMorgan (AND a b) = AND (elimORwMorgan a) (elimORwMorgan b)
elimORwMorgan (XOR a b) = XOR (elimORwMorgan a) (elimORwMorgan b)
elimORwMorgan e = e
