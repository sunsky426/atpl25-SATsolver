module Quantumize where

import AST
import Control.Monad.State
import Data.List (union)
import HQP.QOp.Syntax

data Register
  = Input Int
  | Ancilla Int
  | Output
  deriving(Show, Eq)

-- Operands come before result
data Instr
  = InstrAND Register Register Register
  | InstrXOR Register Register Register
  | InstrNEG Register Register
  | InstrReset Register
  | InstrCopy Register Register
  deriving(Show, Eq)

data CompilerState = CompilerState {
  next :: Int,
  free :: [Register],
  instrs :: [Instr]
}

type Compiler = State CompilerState

alloc :: Compiler Register
alloc = do
  CompilerState next free instrs <- get
  case free of
    [] -> do
      put $ CompilerState (next + 1) free instrs
      pure $ Ancilla next
    q:qs -> do
      put $ CompilerState next qs instrs
      pure q

emit :: Instr -> Compiler ()
emit instr = do
  cs <- get
  put $ cs {instrs = instrs cs ++ [instr]}

dealloc :: Register -> Compiler ()
dealloc toFree = do
  emit $ InstrReset toFree
  cs <- get
  put $ cs {free = free cs `union` [toFree]}

compile' :: Exp -> Compiler Register
compile' (Var n) = pure $ Input n
compile' (AND e1 e2) = do
  in1 <- compile' e1
  in2 <- compile' e2
  out <- alloc
  emit $ InstrAND in1 in2 out
  dealloc in1
  dealloc in2
  pure out
compile' (OR e1 e2) = compile' $ elimORwXOR $ OR e1 e2
compile' (XOR e1 e2) = do
  in1 <- compile' e1
  in2 <- compile' e2
  out <- alloc
  emit $ InstrXOR in1 in2 out
  dealloc in1
  dealloc in2
  pure out
compile' (NEG e) = do
  input <- compile' e
  out <- alloc
  emit $ InstrNEG input out
  dealloc input
  pure out

initialState :: CompilerState
initialState = CompilerState { next = 0, free = [], instrs = [] }

compile :: Exp -> ([Instr], Int)
compile expr = (instrs result, next result)
  where
    (_, result) = runState compiler initialState
    compiler = do
      out <- compile' expr
      emit $ InstrCopy out Output
      dealloc out

type CircuitWidth = Int

pow :: QOp -> Int -> QOp
pow op p = op <.> pow op (p-1)

reorder :: (Int, Int) -> (Int, Int)
reorder (i, j) = if i > j then (j, i) else (i, j)

swap :: CircuitWidth -> (Int, Int) -> QOp
swap n (i0, j0) = Permute $ [0..i-1] ++ [j] ++ [i+1..j-1] ++ [i] ++ [j+1..n-1]
  where (i, j) = reorder (i0, j0)

ccx :: CircuitWidth -> Int -> Int -> Int -> QOp
ccx n i j k = swap n (i, k-2) <> swap n (j, k-1) <> pow I (k-3) <.> C (C X) <.> pow I (n - k) <> swap n (i, k-2) <> swap n (j, k-1)

cx :: CircuitWidth -> Int -> Int -> QOp
cx n i j = swap n (i, j-1) <> pow I (j-2) <.> C X <.> pow I (n - j) <> swap n (i, j-1)

registerToPos :: (Int, Int) -> Register -> Int
registerToPos _ (Input i) = i
registerToPos (n, _) (Ancilla i) = n + i
registerToPos (n, m) Output = n + m

quantumize :: (Int, Int) -> [Instr] -> Program
quantumize _ [] = []
quantumize nm@(n, m) (instr : instrs) = operation ++ quantumize (n, m) instrs
  where
    width = n + m + 1
    operation = case instr of
      InstrAND in1 in2 out -> 
        [Unitary $ ccx width (registerToPos nm in1) (registerToPos nm in2) (registerToPos nm out)]
      InstrXOR in1 in2 out -> 
        [Unitary $ cx width (registerToPos nm in1) (registerToPos nm out) <> cx width (registerToPos nm in2) (registerToPos nm out)]
      InstrNEG input output -> 
        [Unitary $ cx width (registerToPos nm input) (registerToPos nm output)]
      InstrReset register ->
        [Measure [registerToPos nm register], undefined]
      InstrCopy from to -> 
        [Unitary $ cx width (registerToPos nm from) (registerToPos nm to)]