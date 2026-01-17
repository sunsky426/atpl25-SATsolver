module GenEval.Gates where

import qualified Data.Set as S

-- the building bricks of quantum simulation.
type Pos = Int

-- Equivalently the below could be something like `data Op = I Pos | X Pos | ...`.
data Op
  = I | X | Y | Z | H
  deriving (Show, Eq)

data Gate
  = Sing Op (S.Set Int)
  | Ctrl Op (S.Set Int) Int
  deriving (Show,Eq)

type Circuit = [Gate]
