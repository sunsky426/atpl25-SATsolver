{-# LANGUAGE DerivingStrategies #-}
module LinAlg(Qubit, qubit, evalSingle, qfst, qsnd, ApproxEq(..), C, (/^), (*^), setQubit, ppComplex) where

import Gates
import Numeric.LinearAlgebra

class ApproxEq a where
  approxEq :: Double -> a -> a -> Bool
  
  (~=) :: a -> a -> Bool
  a ~= b = approxEq 1e-9 a b

instance ApproxEq Double where
  approxEq tolerence a b = abs (a - b) < tolerence

instance ApproxEq C where
  approxEq tolerence a b = magnitude (a - b) < tolerence

showImag :: Double -> String
showImag 1  = "i"
showImag (-1) = "-i"
showImag x  = show x ++ "i"

ppComplex :: Complex Double -> String
ppComplex (a :+ b)
  | b ~= 0    = show a
  | a ~= 0    = showImag b
  | b > 0     = show a ++ " + " ++ showImag b
  | otherwise = show a ++ " - " ++ showImag (-b)

instance ApproxEq (Vector C) where
  approxEq tolerence v w = size v == size w && norm_2 (v - w) < tolerence

newtype Qubit = Qubit (Vector C)
  deriving newtype (Num, ApproxEq)

instance Show Qubit where
  show qb = "⟨" ++ ppComplex (qfst qb) ++ " ," ++ ppComplex (qsnd qb) ++ "⟩"

-- wrote some constructors and utility functions for Qubit 
-- because I don't want to use hmatrix directly in Eval Module
qubit :: C -> C -> Qubit
qubit a b = Qubit (2 |> [a, b])

(*^) :: C -> Qubit -> Qubit
z *^ (Qubit v) = Qubit $ scale z v

(/^) :: Qubit -> Qubit -> Maybe C
qb1 /^ qb2 =
  if (qfst qb1 / qfst qb2) == (qsnd qb1 / qsnd qb2)
    then Just $ qfst qb1 / qfst qb2
    else Nothing

qfst :: Qubit -> C
qfst (Qubit v) = v ! 0

qsnd :: Qubit -> C
qsnd (Qubit v) = v ! 1

setQubit :: (C -> C) -> (C -> C) -> Qubit -> Qubit
setQubit f g qb = Qubit $ 2 |> [f (qfst qb), g (qsnd qb)]

ii :: C
ii = 0 :+ 1

-- toVector :: Qubit -> Vector (C)
-- toVector (Qubit a b) = 2 |> [a, b]

-- toQubit :: Vector (C) -> Qubit
-- toQubit v = Qubit (v ! 0) (v ! 1)

-- evaluates the effect of a single gate on a single qubit useing Matrix Semantics (I copied this from James)
evalSingle  :: SingleGate -> Qubit -> Qubit
evalSingle gate (Qubit qb) = Qubit $ mat #> qb
  where
    mat = case gate of
      I -> (2 >< 2) [1,0,
                    0,1]

      X -> (2 >< 2) [0,1,
                    1,0]

      Y -> (2 >< 2) [-ii, 0,
                      0, ii]

      Z -> (2 >< 2) [1,0,
                    0,-1]

      H -> let s = 1/sqrt 2
          in  s * (2><2) [1, 1,
                          1,-1]