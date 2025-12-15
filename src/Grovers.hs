module Grovers where

import Quantumize
import HQP.QOp.Syntax
import Programs.QFT

-- make oracle a phase oracle (flip sign if solution)
prepareOracle :: Int -> QOp
prepareOracle 0 = One
prepareOracle n = cx n 0 n >: prepareOracle (n-1) <.> I

-- multi control Z gate
mcz :: Int -> QOp
mcz 1 = Z
mcz n = C $ mcz $ n-1

-- repeat a gate n times
repeatQ :: QOp -> Int -> QOp
repeatQ op 1 = op
repeatQ op n = op <> repeatQ op (n-1)

-- diffussion step (reflect across the equal superposition vector)
diffusion :: CircuitWidth -> QOp
diffusion width = pow H width >: pow X width >: mcz width >: pow X width >: pow H width

-- grovers algorithm (n should be equal to floor(sqrt(N/M)))
grovers :: CircuitWidth -> QOp -> Int -> QOp
grovers width _ 0 = pow H width
grovers width oracle n = oracle >: prepareOracle width >: diffusion width >: grovers width oracle (n-1)

-- quantum counting algorithm
countQ :: CircuitWidth -> QOp -> Int -> QOp
countQ width oracle = estimatePhase (pow H width) oracle width

-- quantum phase estimation algorithm
estimatePhase :: QOp -> QOp -> Int -> Int -> QOp
estimatePhase eigenVector linTrans n m =
  (foldl (>:) (pow H m <.> eigenVector) [step i | i <- [0..m-1]]) >: iQFT width
    where
      width = n + m
      step i = swap width (i, m) >: pow I (m-1) <.> repeatQ (C linTrans) i >: swap width (i, m)

-- inverse quantum fourier transform
iQFT :: CircuitWidth -> QOp
iQFT width = Adjoint $ qft width