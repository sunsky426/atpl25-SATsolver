module Gates where

-- the building bricks of quantum simulation.
type QubitPos = Int

type CircuitWidth = Int

data SingleGate
  = X | H
  deriving(Show)

data Gate 
  = Only QubitPos SingleGate
  | MCZ [QubitPos]
  deriving(Show)

type Program = [Gate]

pow :: SingleGate -> Int -> Program
pow gate n = map (`Only` gate) [0..n-1]