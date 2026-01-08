module Tests where

import AST
import ANF
import Verif
import Test.QuickCheck
import Eval (evalProgram, zero)
import Measure (vectorize, greedyMeasure, toBin)
import Grovers (grovers)
import Gates (CircuitWidth)

maxVar :: Exp -> Int
maxVar (Atom (Var n)) = n
maxVar (Atom (Cst _)) = -1
maxVar (AND a b) = max (maxVar a) (maxVar b)
maxVar (XOR a b) = max (maxVar a) (maxVar b)
maxVar (OR a b) = max (maxVar a) (maxVar b)
maxVar (NEG a) = maxVar a

genBitStrings :: Int -> Gen BitString
genBitStrings n = vectorOf n arbitrary

exp2anfTest :: Exp -> Property
exp2anfTest e =
  forAll (genBitStrings (maxVar e + 1)) $
    \bs -> case exp2anf e of
      Nothing -> False
      Just anf -> verif e bs == verifANF anf bs

groverTest :: Exp -> Property
groverTest e = 
  let width = maxVar e + 1
  in width /= 0 ==> property $
    case runGrover width e of
      Nothing -> False
      Just solution -> verif e solution

runGrover :: CircuitWidth -> Exp -> Maybe BitString
runGrover width e = do
  oracle <- anf2oracle <$> exp2anf e
  let groversCircuit = grovers width oracle 1
  let solution = toBin $ greedyMeasure $ vectorize $ evalProgram groversCircuit (zero width)
  pure solution
