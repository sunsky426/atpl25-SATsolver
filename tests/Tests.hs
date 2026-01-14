module Tests where

import AST
import ANF
import Verif
import Test.QuickCheck
import Eval (evalProgram, zero, scanProgram)
import Measure (vectorize, seperateSolution)
import Grovers (grovers)
import Gates (CircuitWidth)
import Data.List (nub)
import Data.Vector.Storable (Vector)
import Data.Vector.Unboxed.Mutable (MVector(MV_2))

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

groverCheatTest :: Exp -> Property
groverCheatTest e = 
  let width = maxVar e + 1
  in width > 0 ==> property $
    case runGroverCheat width e of
      Nothing -> False
      Just (solSet1, solSet2) ->
        case (nub $ verif e <$> solSet1, nub $ verif e <$> solSet2) of
          ([truthVal1], [truthVal2]) -> truthVal1 /= truthVal2
          ([_], _) -> True
          (_, [_]) -> True
          _ -> False

runGroverCheat :: CircuitWidth -> Exp -> Maybe ([BitString], [BitString])
runGroverCheat width e = do
  oracle <- anf2oracle <$> exp2anf e
  let groversCircuit = grovers width oracle 1
  let stateVector = vectorize $ evalProgram groversCircuit (zero width)
  pure $ (pairMap . map) (padded2bin width) (seperateSolution stateVector)

pairMap :: (a -> b) -> (a, a) -> (b, b)
pairMap f (x, y) = (f x, f y)

runGrover :: CircuitWidth -> Exp -> Maybe (Vector Double)
runGrover width e = do
  oracle <- anf2oracle <$> exp2anf e
  let groversCircuit = grovers width oracle 1
  let solution = vectorize $ evalProgram groversCircuit (zero width)
  pure solution

--- 

tensorRankTest :: Exp -> Property
tensorRankTest e = 
  let width = maxVar e + 1
  in forAll (getPositive <$> arbitrary) $ \iterations ->
    case (scanGrover width e 1, scanGrover width e iterations) of
      (Nothing, _) -> False
      (_, Nothing) -> False
      (Just ranks1, Just ranks2) -> maximum ranks1 == maximum ranks2

scanGrover :: CircuitWidth -> Exp -> Int -> Maybe [Int]
scanGrover width e iterations = do
  oracle <- anf2oracle <$> exp2anf e
  let groversCircuit = grovers width oracle iterations
  let ranks = scanProgram groversCircuit (zero width)
  pure ranks