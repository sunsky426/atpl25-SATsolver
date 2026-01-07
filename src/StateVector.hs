module StateVector where

import LinAlg

import Data.Vector as V
import Data.List as L

import Eval

import Numeric.LinearAlgebra as NL

tensorToStateVector :: Tensor -> NL.Vector C
tensorToStateVector (PT {Eval.scalar = s, qbs} : tl) =
        mul' tl $ outerWithFlatten ((1 |> [s]) * (unQubit . V.head) qbs) $ V.map unQubit $ V.tail qbs
    where
        outerWithFlatten :: NL.Vector C -> V.Vector (NL.Vector C) -> NL.Vector C
        outerWithFlatten v vs = V.foldl (\a b -> flatten $ outer a b) v vs
        mul' [] res = res
        mul' (PT {Eval.scalar = s, qbs} : rest) res =
            mul' rest $ (+ res) $ outerWithFlatten ((1 |> [s]) * (unQubit . V.head) qbs) $  V.map unQubit $ V.tail qbs
        mul' (_:tl) res = mul' tl res
        
tensorToStateVector _ = 0 |> []

toBin 0 = "0"
toBin 1 = "1"
toBin n = 
    if n `mod` 2 == 1 then  toBin (n `div` 2) Prelude.++ "1"
    else toBin (n `div` 2)  Prelude.++ "0"

ppSV v = fst $ L.foldl (\(s, n) e -> (s Prelude.++ toBin n Prelude.++ ": " Prelude.++ show e Prelude.++ "\n", n+1)) ("", 0) $ NL.toList v
