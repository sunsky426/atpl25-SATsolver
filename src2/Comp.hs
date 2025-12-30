module Comp where

import AST
import Parser
--import Control.Monad.State

-- the building bricks of quantum simulation.
type Pos = Int

-- Equivalently the below could be something like `data Op = I Pos | X Pos | ...`.
data Op
  = I | X | Y | Z | H
  deriving (Show)

data Gate
  = Single Op Pos
  -- | C [Pos] Op Pos
  | CZ [Pos]
  deriving (Show)

type QP = [Gate]

data ANF
  = Cst Bool
  | Pos Int
  | Xor ANF ANF
  | And ANF ANF
  deriving (Eq, Show)

astToAnf :: Exp -> ANF
astToAnf (Const b) = Cst b
astToAnf (Var i) = Pos i
astToAnf (NEG e) = Xor (astToAnf e) (Cst True)
astToAnf (XOR e1 e2) = Xor (astToAnf e1) (astToAnf e2)
astToAnf (OR e1 e2) =
  let e1' = astToAnf e1
      e2' = astToAnf e2
   in Xor (Xor e1' e2') (And e1' e2') -- equivalent to elimORwXOR
astToAnf (AND e1 e2) = And (astToAnf e1) (astToAnf e2)

-- distribute And across Xor.
distributeAnd :: ANF -> ANF
distributeAnd anf =
  case anf of
    And e1 e2 -> distAnd (dist e1) (dist e2)
    Xor e1 e2 -> Xor (dist e1) (dist e2)
    _ -> anf
  where 
    dist a = distributeAnd a
    distAnd e1 e2 =
      case (e1,e2) of
        (Xor a b,c) -> Xor (distAnd a c) (distAnd b c)
        (a,Xor b c) -> Xor (distAnd a b) (distAnd a c)
        _ -> And e1 e2

buildPhase :: ANF -> QP
buildPhase (Pos i) = [CZ [i]]
buildPhase (Cst _) = []
buildPhase (Xor e1 e2) = buildPhase e1 ++ buildPhase e2
buildPhase (And e1 e2) = [CZ (collectPos e1 ++ collectPos e2)]
  where
    collectPos anf =
      case anf of
        Pos i -> [i]
        And a b -> collectPos a ++ collectPos b
        _ -> []

phaseOracle :: Exp -> QP
phaseOracle e = buildPhase . distributeAnd . astToAnf $ e

--type AT = State Int -- AT for ancilla tracker
--
--inc :: AT ()
--inc = modify (+1)
--
--allocAncilla :: AT Int
--allocAncilla = do
--  pos <- get
--  inc
--  pure pos
--
--buildQP :: Exp -> AT (QP,Pos)
--buildQP (XOR e1 e2) = do
--  (qp1,p1) <- buildQP e1
--  (qp2,p2) <- buildQP e2
--  pos1 <- allocAncilla
--  pos2 <- allocAncilla
--  pure ((C [p2,pos1] X pos2) : (C [p1] X pos1) : qp1 ++ qp2,pos2)
--buildQP (AND e1 e2) = do
--  (qp1,p1) <- buildQP e1
--  (qp2,p2) <- buildQP e2
--  pos <- allocAncilla
--  pure ((C [p1,p2] X pos) : qp1 ++ qp2,pos)
--buildQP (NEG e) = do
--  (qp,p) <- buildQP e
--  pos <- allocAncilla
--  pure ((C [p] X pos) : qp,pos)
--buildQP (OR e1 e2) = buildQP $ elimORwMorgan $ OR e1 e2
--buildQP (Var i) = pure ([],i)
--
--quantumize :: (Exp,Int) -> QP
--quantumize (e,i) = reverse $ fst $ fst $ runState (buildQP e) i
