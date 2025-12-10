module Quantumize where

import AST
import HQP.QOp.Operator
import Control.Monad.State
import Data.List (union)

data QuRegistr
  = Input Int
  | Ancilla Int
  | Output
  deriving(Show, Eq)

-- Operands come before result
data Instr 
  = InstrAND QuRegistr QuRegistr QuRegistr 
  | InstrOR QuRegistr QuRegistr QuRegistr
  | InstrXOR QuRegistr QuRegistr QuRegistr 
  | InstrNEG QuRegistr QuRegistr
  | InstrReset QuRegistr
  | InstrCopy QuRegistr QuRegistr
  deriving(Show, Eq)

data CompilerState = CompilerState {
  next :: Int,
  free :: [QuRegistr],
  instrs :: [Instr]
}

type Compiler = State CompilerState

alloc :: Compiler QuRegistr
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

dealloc :: QuRegistr -> Compiler ()
dealloc toFree = do
  emit $ InstrReset toFree
  cs <- get
  put $ cs {free = free cs `union` [toFree]}

compile' :: Exp -> Compiler QuRegistr
compile' (Var n) = pure $ Input n
compile' (AND e1 e2) = do
  in1 <- compile' e1 
  in2 <- compile' e2
  out <- alloc
  emit $ InstrAND in1 in2 out
  dealloc in1
  dealloc in2
  pure out
compile' (OR e1 e2) = do
  in1 <- compile' e1 
  in2 <- compile' e2
  out <- alloc
  emit $ InstrOR in1 in2 out
  dealloc in1
  dealloc in2
  pure out
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

quantumize :: Exp -> Operator
quantumize = undefined