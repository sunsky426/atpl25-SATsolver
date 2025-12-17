module Eval(evalProgram) where

import Gates
import LinAlg
import qualified Data.Vector as V
import Data.List (tails)

type PureTensor = V.Vector Qubit

type Tensor = [PureTensor]

-- |0..0> in tensor notation
zero :: Int -> Tensor
zero dim = [V.replicate dim (qubit 1 0)]

-- apply all gates in a program sequentiall (via fold)
evalProgram :: Program -> Tensor -> Tensor
evalProgram program tensor = foldl (flip evalGate) tensor program

-- distributes a gate over addition to all pure tensors in a tensor
evalGate :: Gate -> Tensor -> Tensor
evalGate gate = concatMap (evalTerm gate)

-- evaluates a gate on a pure tensor, using LinAlg Module
evalTerm :: Gate -> PureTensor -> Tensor
evalTerm (Only pos gate) qbs = pure $ qbs V.// [(pos, evalSingle gate (qbs V.! pos))]
evalTerm (Ctrl ctrls target gate) qbs = [qbs, correction]
  where
    dimension = length qbs
    beta = product $ [qsnd (qbs V.! i) | i <- ctrls]
    targetVal' = beta *^ (evalSingle gate (qbs V.! target) - (qbs V.! target))
    correction = V.replicate dimension (qubit 0 0) V.// ((target, targetVal') : zip ctrls (repeat (qubit 0 1)))

tensorSimp :: Tensor -> Tensor
tensorSimp t = concatMap f [(x, y) | (x : ys) <- tails t, y <- ys]
  where 
    f (x, y) = 
      case pureTensorSimp x y of
        Just pureTensor -> [pureTensor]
        Nothing -> [x, y]


pureTensorSimp :: PureTensor -> PureTensor -> Maybe PureTensor
pureTensorSimp t1 t2 = do
  let isEq = V.zipWith (~=) t1 t2
  let lastIndex = length t1 - 1
  firstFalseIndex <- V.findIndex not isEq
  case countFalse isEq of
    0 -> Just $ t1 V.// [(lastIndex, 2 *^ (t1 V.! lastIndex))]
    1 -> Just $ t1 V.// [(firstFalseIndex, (t1 V.! firstFalseIndex) + (t2 V.! firstFalseIndex))]
    _ -> Nothing

countFalse :: V.Vector Bool -> Int
countFalse = V.length . V.filter not