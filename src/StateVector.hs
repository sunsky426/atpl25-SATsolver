module StateVector where

import LinAlg

import Data.Vector as V

import Eval

import Numeric.LinearAlgebra as NL

tensorToStateVector :: Tensor -> NL.Vector CC
tensorToStateVector (PT {Eval.scalar = s, qbs} : tl) =
        mul' tl $ outerWithFlatten ((1 |> [s]) * (unQubit . V.head) qbs) $ V.map unQubit $ V.tail qbs
    where
        outerWithFlatten :: NL.Vector CC -> V.Vector (NL.Vector CC) -> NL.Vector CC
        outerWithFlatten v vs = V.foldl (\a b -> flatten $ outer a b) v vs
        mul' [] res = res
        mul' (PT {Eval.scalar = s, qbs} : rest) res =
            mul' rest $ (+ res) $ outerWithFlatten ((1 |> [s]) * (unQubit . V.head) qbs) $  V.map unQubit $ V.tail qbs
        mul' (_:tl) res = mul' tl res
        
tensorToStateVector _ = 0 |> []
