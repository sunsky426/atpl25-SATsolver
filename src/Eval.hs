module Eval where

import Data.Complex
import Gates

type Qubit = (Complex Double, Complex Double)
newtype PureTensor a = PureTensor [a]
newtype Tensor a = Tensor [PureTensor a]

eval :: Program -> Tensor Qubit -> Tensor Qubit
eval = undefined

evalTerm :: Program -> PureTensor Qubit -> Tensor Qubit
evalTerm = undefined
