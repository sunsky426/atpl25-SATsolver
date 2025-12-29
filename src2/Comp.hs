module Comp where

import AST
import Control.Monad.State

-- the building bricks of quantum simulation.
type Pos = Int

-- Equivalently the below could be something like `data Op = I Pos | X Pos | ...`.
data Op
  = I | X | Y | Z | H
  deriving (Show)

data Gate
  = Single Op Pos
  | C [Pos] Op Pos
  deriving (Show)

type QP = [Gate]
type AT = State Int -- AT for ancilla tracker

inc :: AT ()
inc = modify (+1)

allocAncilla :: AT Int
allocAncilla = do
  pos <- get
  inc
  pure pos

buildQP :: Exp -> AT (QP,Pos)
buildQP (XOR e1 e2) = do
  (qp1,p1) <- buildQP e1
  (qp2,p2) <- buildQP e2
  pos1 <- allocAncilla
  pos2 <- allocAncilla
  pure ((C [p2,pos1] X pos2) : (C [p1] X pos1) : qp1 ++ qp2,pos2)
buildQP (AND e1 e2) = do
  (qp1,p1) <- buildQP e1
  (qp2,p2) <- buildQP e2
  pos <- allocAncilla
  pure ((C [p1,p2] X pos) : qp1 ++ qp2,pos)
buildQP (NEG e) = do
  (qp,p) <- buildQP e
  pos <- allocAncilla
  pure ((C [p] X pos) : qp,pos)
buildQP (OR e1 e2) = buildQP $ elimORwMorgan $ OR e1 e2
buildQP (Var i) = pure ([],i)

quantumize :: (Exp,Int) -> QP
quantumize (e,i) = reverse $ fst $ fst $ runState (buildQP e) i
