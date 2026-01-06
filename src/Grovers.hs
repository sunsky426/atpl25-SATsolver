module Grovers(grovers, countQ) where

import Programs.QFT
import Macros
import Gates

-- make oracle a phase oracle (flip sign if solution) WRONG
-- prepareOracle :: CircuitWidth -> Program
-- prepareOracle width = map (\i -> Ctrl [0] i X) [1..width-1]

-- diffussion step (reflect across the equal superposition vector)
diffusion :: CircuitWidth -> Program
diffusion width = pow H width ++ pow X width ++ [MCZ [0..width-1]] ++ pow X width ++ pow H width

-- grovers algorithm (n should be equal to floor(sqrt(N/M)), find M using the quantum counting algorithm)
grovers :: CircuitWidth -> Program -> Int -> Program
grovers width oracle n = pow H width ++ concat (replicate n (oracle ++ diffusion width))

-- quantum counting algorithm
countQ :: CircuitWidth -> Program -> Int -> Program
countQ width oracle = estimatePhase (pow H width) oracle width

-- quantum phase estimation algorithm
estimatePhase :: Program -> Program -> Int -> Int -> Program
-- estimatePhase eigenVector linTrans n m =
--   (foldl (>:) (pow H m <.> eigenVector) [step i | i <- [0..m-1]]) >: iQFT width
--     where
--       width = n + m
--       step i = swap width (i, m) >: pow I (m-1) <.> repeatQ (C linTrans) i >: swap width (i, m)
estimatePhase = undefined

-- inverse quantum fourier transform
iQFT :: CircuitWidth -> Program
iQFT width = undefined