module Grovers where

import Gates

-- diffussion step (reflect across the equal superposition vector)
diffusion :: CircuitWidth -> Program
diffusion width = pow H width ++ pow X width ++ [MCZ [0..width-1]] ++ pow X width ++ pow H width

groverIteration :: Program -> Program -> Int -> Program
groverIteration oracle diffuser 1 = oracle ++ diffuser
groverIteration oracle diffuser n =
  oracle ++ diffuser ++ groverIteration oracle diffuser (n-1)

grover :: Program -> Int -> Program
grover oracle n =
  pow H n ++ groverIteration oracle (diffusion n) (floor (pi / 4.0 * sqrt(2 ^ n)))
