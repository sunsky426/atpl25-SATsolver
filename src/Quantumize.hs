module Quantumize(compile, quantumize) where

import AST
import Control.Monad.State
import Gates

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
  | InstrCopy Register Register
  deriving(Show, Eq)

data CompilerState = CompilerState {
  next :: Int,
  instrs :: [Instr]
}

type Compiler = State CompilerState

alloc :: Compiler Register
alloc = do
  CompilerState next instrs <- get
  put $ CompilerState (next + 1) instrs
  pure $ Ancilla next

emit :: [Instr] -> Compiler ()
emit toAdd = do
  cs <- get
  put $ cs {instrs = instrs cs ++ toAdd}

-- compiling logical expression to an internal imperative language
compile' :: Exp -> Compiler Register
compile' (Var n) = pure $ Input n
compile' (AND e1 e2) = do
  in1 <- compile' e1
  in2 <- compile' e2
  out <- alloc
  emit [InstrAND in1 in2 out]
  pure out
compile' (OR e1 e2) = compile' $ elimORwMorgan $ OR e1 e2
compile' (XOR e1 e2) = do
  in1 <- compile' e1
  in2 <- compile' e2
  out <- alloc
  emit [InstrXOR in1 in2 out]
  pure out
compile' (NEG e) = do
  input <- compile' e
  out <- alloc
  emit [InstrNEG input out]
  pure out

initialState :: CompilerState
initialState = CompilerState { next = 0, instrs = [] }

compile :: Exp -> ([Instr], Int)
compile expr = (instrs result, next result)
  where
    (_, result) = runState compiler initialState
    compiler = do
      out <- compile' expr
      CompilerState _ instrs <- get
      emit [InstrCopy out Output]
      emit $ reverse instrs

-- register name to qubit number
registerToPos :: (Int, Int) -> Register -> Int
registerToPos _ Output = 0
registerToPos _ (Input i) = i + 1
registerToPos (n, _m) (Ancilla i) = n + i

-- convert internal language to quantum circuit
quantumize :: (Int, Int) -> [Instr] -> QP
quantumize _ [] = []
quantumize (n, m) (instr : instrs) = quantumize (n, m) instrs ++ operation
  where
    operation = case instr of
      InstrAND in1 in2 out -> 
        [C [registerToPos (n, m) in1, registerToPos (n, m) in2] (registerToPos (n, m) out) X]
      InstrXOR in1 in2 out -> 
        [C [registerToPos (n, m) in1] (registerToPos (n, m) out) X, 
          C [registerToPos (n, m) in2] (registerToPos (n, m) out) X]
      InstrNEG input output -> 
        [C [registerToPos (n, m) input] (registerToPos (n, m) output) X]
      InstrCopy from to -> 
        [C [registerToPos (n, m) from] (registerToPos (n, m) to) X]
