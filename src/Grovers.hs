module Grovers(grovers, countQ) where

import HQP.QOp.Syntax
import Programs.QFT
import Macros

-- make oracle a phase oracle (flip sign if solution)
prepareOracle :: Int -> QOp
prepareOracle 0 = One
prepareOracle n = cx n 0 n >: prepareOracle (n-1) <.> I

-- diffussion step (reflect across the equal superposition vector)
diffusion :: CircuitWidth -> QOp
diffusion width = pow H width >: pow X width >: mcz width >: pow X width >: pow H width

-- grovers algorithm (n should be equal to floor(sqrt(N/M)), find M using the quantum counting algorithm)
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