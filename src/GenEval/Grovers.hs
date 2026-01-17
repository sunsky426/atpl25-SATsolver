module GenEval.Grovers where

import GenEval.Gates
import qualified Data.Set as S

pow :: Op -> Int -> Circuit
pow op i = map (\x -> Sing op (S.singleton x)) [0..i-1]

diffusion :: Int -> Circuit
diffusion n =
  pow H n ++
  pow X n ++
  [Ctrl Z (S.fromList [0..n-2]) (n-1)] ++
  pow X n ++
  pow H n

groverIteration :: Circuit -> Circuit -> Int -> Circuit
groverIteration oracle diffuser 1 = oracle ++ diffuser
groverIteration oracle diffuser n =
  oracle ++ diffuser ++ groverIteration oracle diffuser (n-1)

grover :: Circuit -> Int -> Circuit
grover oracle n =
  pow H n ++ groverIteration oracle (diffusion n) (floor ((pi / 4.0 * sqrt (2 ^ n)) :: Double))
