module GenEval.Grovers where

import GenEval.Gates

pow :: Op -> Int -> QP
pow op i = map (Single op) [0..i-1]

diffusion :: Int -> QP
diffusion n =
  pow H n ++
  pow X n ++
  [CZ [0..n-1]] ++
  pow X n ++
  pow H n

groverIteration :: QP -> QP -> Int -> QP
groverIteration oracle diffuser 1 = oracle ++ diffuser
groverIteration oracle diffuser n =
  oracle ++ diffuser ++ groverIteration oracle diffuser (n-1)

grover :: QP -> Int -> QP
grover oracle n =
  pow H n ++ groverIteration oracle (diffusion n) (floor ((pi / 4.0 * sqrt (2 ^ n)) :: Double))
