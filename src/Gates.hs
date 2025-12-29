module Gates where

type QubitPos = Int

data SingleGate
  = I | X | Y | Z | H
  deriving(Show)

data Gate 
  = Only QubitPos SingleGate
  | Ctrl [QubitPos] QubitPos SingleGate
  deriving(Show)

type Program = [Gate]