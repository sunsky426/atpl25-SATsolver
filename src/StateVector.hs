module StateVector where

import LinAlg

import Data.Vector as V
import Data.List as L
import Data.Ord (comparing)
import Data.Bits (countTrailingZeros)

import EvalMV
import Numeric.LinearAlgebra as NL
import Debug.Trace (trace)

getBest (_ : PTIV s qbs : _) =
  L.concatMap (\q -> if (abs $ realPart $ qfst q) > (abs $ realPart $ qsnd q) then "0" else "1") qbs

tensorToStateVector :: [PureTensorIV] -> NL.Vector C
tensorToStateVector pt = tensorToStateVector' pt
  where
    tensorToStateVector' [] = 0
    tensorToStateVector' pt@(PTIV s qbs : t) =
      (1 |> [s]) * V.foldl (\a b -> flatten $ outer a b) (unQubit $ V.head qbs) (V.map unQubit $ V.tail qbs)
      + tensorToStateVector' t

toBin l 0 = L.replicate (l-1) '0' Prelude.++ "0"
toBin l 1 = L.replicate (l-1) '0' Prelude.++ "1"
toBin l n =
    if n `mod` 2 == 1 then  toBin (l-1) (n `div` 2) Prelude.++ "1"
    else toBin (l-1) (n `div` 2)  Prelude.++ "0"

ppSV v = fst $ L.foldl (\(s, n) e -> (s Prelude.++ toBin (round $ logBase (fromIntegral 2) (fromIntegral $ NL.size v)) n Prelude.++ ": " Prelude.++ show (e ** 2) Prelude.++ "\n", n+1)) ("", 0) $ NL.toList v
