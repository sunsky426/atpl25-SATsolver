module Gates where

-- the building bricks of quantum simulation.
type Pos = Int

-- Equivalently the below could be something like `data Op = I Pos | X Pos | ...`.
data Op
  = I | X | Y | Z | H
  deriving (Show, Eq)

data QGate
  = Single Op Pos
  | C [Pos] Pos Op
  | CZ [Pos]
  deriving (Show)

type QP = [QGate]
