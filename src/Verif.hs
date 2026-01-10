module Verif where

import AST

type BitString = [Bool]

xor :: Bool -> Bool -> Bool
xor True True = False
xor False False = False
xor _ _ = True


verif :: Exp -> BitString -> Bool
verif (Const b) _ = b
verif (Var n) bs = bs !! n
verif (AND a b) bs = verif a bs && verif b bs
verif (OR a b) bs = verif a bs || verif b bs
verif (XOR a b) bs = verif a bs `xor` verif b bs
verif (NEG a) bs = not $ verif a bs