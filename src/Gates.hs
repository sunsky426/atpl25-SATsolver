module Gates where

data SingleGate
  = I | X | Y | Z | H

data Gate 
  = Only SingleGate
  | Ctrl Int SingleGate

newtype Program = Program [Gate]