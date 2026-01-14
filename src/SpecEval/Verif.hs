module SpecEval.Verif where

import SpecEval.AST
import SpecEval.ANF

type BitString = [Bool]

xor :: Bool -> Bool -> Bool
xor True True = False
xor False False = False
xor _ _ = True

verif :: Exp -> BitString -> Bool
verif (Atom (Cst b)) _ = b
verif (Atom (Var n)) bs = bs !! n
verif (AND a b) bs = verif a bs && verif b bs
verif (OR a b) bs = verif a bs || verif b bs
verif (XOR a b) bs = verif a bs `xor` verif b bs
verif (NEG a) bs = not $ verif a bs

verifANF :: ANF -> BitString -> Bool
verifANF anf bs =
  odd $ length $ filter id $ map verifTerm anf
  where
    verifTerm :: ANFterm -> Bool
    verifTerm = all verifAtom
    verifAtom :: Atom -> Bool
    verifAtom (Cst b) = b
    verifAtom (Var n) = bs !! n

--- int to bitstring ---

padded2bin :: Int -> Int -> BitString
padded2bin n i = 
  let unpadded = toBin i
  in replicate (n - length unpadded) False ++ unpadded

toBin :: Int -> BitString
toBin 0 = [False]
toBin 1 = [True]
toBin n = 
    if n `mod` 2 == 1 then toBin (n `div` 2) ++ [True]
    else toBin (n `div` 2) ++ [False]