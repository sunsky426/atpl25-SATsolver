module Macros where

import HQP.QOp.Syntax

type CircuitWidth = Int

pow :: QOp -> Int -> QOp
pow _ 0 = One
pow op p = op <.> pow op (p-1)

reorder :: (Int, Int) -> (Int, Int)
reorder (i, j) = if i > j then (j, i) else (i, j)

swap :: CircuitWidth -> (Int, Int) -> QOp
swap n (i0, j0) = Permute $ [0..i-1] ++ [j] ++ [i+1..j-1] ++ [i] ++ [j+1..n-1]
  where (i, j) = reorder (i0, j0)

ccx :: CircuitWidth -> Int -> Int -> Int -> QOp
ccx n i j k =
  swap n (i, 0) <> swap n (j, 1) <> swap n (k, 2) <> C (C X) <.> pow I (n - 3) <> swap n (i, 0) <> swap n (j, 1) <> swap n (k, 2) 

cx :: CircuitWidth -> Int -> Int -> QOp
cx n i j = 
  swap n (i, 0) <> swap n (j, 1) <> C X <.> pow I (n - 2) <> swap n (i, 0) <> swap n (j, 1)

-- multi control Z gate
mcz :: Int -> QOp
mcz 1 = Z
mcz n = C $ mcz $ n-1

-- repeat a gate n times
repeatQ :: QOp -> Int -> QOp
repeatQ op 1 = op
repeatQ op n = op <> repeatQ op (n-1)