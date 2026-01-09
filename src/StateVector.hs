module StateVector where

import LinAlg

import Data.Vector as V
import Data.List as L

import EvalMV
import Numeric.LinearAlgebra as NL
import Debug.Trace (trace)

prob l = (sqrt . abs . unQubit)

-- tensorToStateVector :: [PureTensorIV] -> NL.Vector C
-- tensorToStateVector :: [PureTensorIV] -> NL.Vector C
-- tensorToStateVector :: [PureTensorIV] -> NL.Vector C
-- tensorToStateVector [PTIV _ qbs] = V.foldl (\a b -> flatten $ outer a b) ((**2) $ unQubit $ V.head qbs) (V.map ((**2) . unQubit) $ V.tail (trace (show qbs) qbs))
-- tensorToStateVector pt@(PTIV s1 qbs1 : PTIV s2 qbs2 : t) =
--   tensorToStateVector $ PTIV 1 (V.zipWith (\q1 q2 -> 0.5 *^ (s1 *^ q1 + s2 *^ q2)) qbs1 qbs2) : (trace (show pt) t)

tensorToStateVector :: [PureTensorIV] -> NL.Vector C
tensorToStateVector pt = tensorToStateVector' (trace (show pt) pt)
  where 
    tensorToStateVector' [] = 0
    tensorToStateVector' pt@(PTIV s qbs : t) =
      (1 |> [s]) * V.foldl (\a b -> flatten $ outer a b) (unQubit $ V.head qbs) (V.map (unQubit) $ V.tail qbs) 
      + tensorToStateVector t


-- tensorToStateVector :: [PureTensorIV] -> NL.Vector C
-- tensorToStateVector pt@(PTIV s qbs : tl) =
--         mul' tl $ abs $ outerWithFlatten ((1 |> [s]) * (unQubit . V.head) qbs) $ V.map unQubit $ V.tail (trace (show pt) qbs)
--     where
--         mul' [] res = res
--         mul' (PTIV {scalarIV = s, qbsIV = qbs} : rest) res =
--             mul' rest $ (+ res) $ abs $ outerWithFlatten ((1 |> [s]) * (unQubit . V.head) qbs) $ V.map unQubit $ V.tail qbs
--         outerWithFlatten = V.foldl (\a b -> flatten $ outer a b)

tensorToStateVector _ = 0 |> []

toBin 0 = "0"
toBin 1 = "1"
toBin n =
    if n `mod` 2 == 1 then  toBin (n `div` 2) Prelude.++ "1"
    else toBin (n `div` 2)  Prelude.++ "0"

ppSV v = fst $ L.foldl (\(s, n) e -> (s Prelude.++ toBin n Prelude.++ ": " Prelude.++ show (e ** 2) Prelude.++ "\n", n+1)) ("", 0) $ NL.toList v
