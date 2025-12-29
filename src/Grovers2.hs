module Grovers2 where

import Gates

pow :: SingleGate -> Int -> Program
pow op i = map (`Only` op) [0..i-1]

diffusion :: Int -> Program
diffusion n = 
  pow H n ++ 
  pow X n ++ 
  [Ctrl [0..n-2] (n-1) Z] ++ 
  pow X n ++ 
  pow H n

groverIteration :: Program -> Program -> Int -> Program
groverIteration oracle diffuser 1 =  oracle ++ diffuser
groverIteration oracle diffuser n = 
  oracle ++ diffuser ++ groverIteration oracle diffuser (n-1)

grover :: Program -> Int -> Program
grover oracle n = 
  pow H n ++ groverIteration oracle (diffusion n) (floor (pi / 4.0 * sqrt(2 ^ n)))
