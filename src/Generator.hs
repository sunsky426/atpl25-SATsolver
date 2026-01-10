module Generator (gen,genMany) where

import AST
import System.Random

genMany :: Int -> Int -> Int -> [Exp]
genMany seed n size
  | n <= 1 = [gen seed size]
  | otherwise = gen seed size : genMany (seed+1) (n-1) size

gen :: Int -> Int -> Exp
gen seed size = fst (genExp size (mkStdGen seed))

genExp :: Int -> StdGen -> (Exp, StdGen)
genExp size g
  | size <= 1 =
      let (v, g') = randomR (0, 9) g
       in (Var v, g')
  | otherwise =
      let (choice, g1) = randomR (0 :: Int, 4) g
       in case choice of
            0 -> binary AND g1
            1 -> binary OR g1
            2 -> binary XOR g1
            3 -> unary NEG g1
            _ -> binary AND g1
  where
    binary ctor g =
      let sizeL = size `div` 2
          sizeR = size - sizeL - 1
          (l, g1) = genExp sizeL g
          (r, g2) = genExp sizeR g1
       in (ctor l r, g2)
    unary ctor g =
      let (e, g') = genExp (size - 1) g
       in (ctor e, g')
