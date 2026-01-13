module Generator (gen,genMany) where

import AST
import System.Random

genMany :: Int -> Int -> Int -> [Exp]
genMany seed n size
  | n <= 1 = [gen seed size]
  | otherwise = gen seed size : genMany (seed+1) (n-1) size

-- gen :: seed -> num of vars -> Exp
gen :: Int -> Int -> Exp
gen seed numVars =
  fst (build (map Var [0 .. numVars - 1]) (mkStdGen seed))

build :: [Exp] -> StdGen -> (Exp, StdGen)
build [e] g = (e, g)
build es g =
  let (choice, g1) = randomR (0 :: Int, 3) g
  in case choice of
       0 -> combineBinary AND es g1
       1 -> combineBinary OR  es g1
       2 -> combineBinary XOR es g1
       _ -> combineUnary  NEG es g1

combineBinary :: (Exp -> Exp -> Exp) -> [Exp] -> StdGen -> (Exp, StdGen)
combineBinary ctor es g =
  let (i, g1) = randomR (0, length es - 1) g
      (j, g2) = randomR (0, length es - 2) g1
      e1 = es !! i
      e2 = es !! (if j >= i then j + 1 else j)
      rest = removeTwo i j es
   in build (ctor e1 e2 : rest) g2

combineUnary :: (Exp -> Exp) -> [Exp] -> StdGen -> (Exp, StdGen)
combineUnary ctor es g =
  let (i, g1) = randomR (0, length es - 1) g
      e = es !! i
      rest = take i es ++ drop (i + 1) es
   in build (ctor e : rest) g1

removeTwo :: Int -> Int -> [a] -> [a]
removeTwo i j xs =
  [ x | (k, x) <- zip [0..] xs
      , k /= i
      , k /= (if j >= i then j + 1 else j)
  ]
