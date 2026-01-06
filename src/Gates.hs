module Gates where

type QubitPos = Int

data SingleGate
  = X | H
  deriving(Show)

data Gate 
  = Only QubitPos SingleGate
  | MCZ [QubitPos]
  deriving(Show)

type Program = [Gate]