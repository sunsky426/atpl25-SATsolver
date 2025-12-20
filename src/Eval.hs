{-# OPTIONS_GHC -Wno-orphans #-}
module Eval where

import Gates
import LinAlg
import qualified Data.Vector as V
import Macros

-- type definations

data PureTensor = PT {scalar :: CC, qbs :: V.Vector Qubit}
  deriving(Show)

(//) :: PureTensor -> [(Int, Qubit -> Qubit)] -> PureTensor
(PT z vec) // updates = PT z $ vec V.// (phi <$> updates)
  where
    phi :: (Int, Qubit -> Qubit) -> (Int, Qubit)
    phi (index, f) = (index, f (vec V.! index))

(!) :: PureTensor -> Int -> Qubit
(PT _ vec) ! i = vec V.! i

(*^) :: CC -> PureTensor -> PureTensor
alpha *^ (PT z vec) = PT (alpha * z) vec

instance ApproxEq PureTensor where
  approxEq tolerence (PT za va) (PT zb vb) = approxEq tolerence za zb && V.and (V.zipWith (approxEq tolerence) va vb)

type Tensor = [PureTensor]

instance ApproxEq Tensor where
  approxEq tolerence a b = and $ zipWith (approxEq tolerence) a b

-- |0..0> in tensor notation
zero :: Int -> Tensor
zero dim = [PT 1 $ V.replicate dim (qubit 1 0)]

hadamard :: Int -> Tensor
hadamard dim = evalProgram (pow H dim) (zero dim)

-- apply all gates in a program sequentiall (via fold)
evalProgram :: Program -> Tensor -> Tensor
evalProgram program tensor = foldl (flip evalGate) tensor program

-- distributes a gate over addition to all pure tensors in a tensor
evalGate :: Gate -> Tensor -> Tensor
evalGate gate = fixpoint tensorSimp . concatMap (evalTerm gate)

-- evaluates a gate on a pure tensor, using LinAlg Module
evalTerm :: Gate -> PureTensor -> Tensor
evalTerm (Only pos gate) qbs = pure $ qbs // [(pos, evalSingle gate)]
evalTerm (Ctrl ctrls target gate) qbs =
  case product $ [qfst (qbs ! i) | i <- ctrls] of
    0 -> [qbs // [(target, evalSingle gate)]]
    _ -> case product $ [qsnd (qbs ! i) | i <- ctrls] of
      0 -> [qbs]
      beta -> [qbs, correction]
        where
          targetUpdate q = evalSingle gate q - q
          ctrlUpdates = repeat $ const $ qubit 0 1
          correction = (beta *^ qbs) // zip (target : ctrls) (targetUpdate : ctrlUpdates)

tensorSimp :: Tensor -> Tensor
tensorSimp tensor = f tensor [(pureTensorSimp (tensor !! i) (tensor !! j), i, j) | i <- [0..size - 1], j <- [i+1..size-1]]
  where
    size = length tensor
    f :: Tensor -> [(Maybe PureTensor, Int, Int)] -> Tensor
    f t [] = t
    f t (update : updates) =
      case update of
        (Nothing, _, _) -> f t updates
        (Just pt', i, j) -> pt' : deleteAtTwo (i, j) t

pureTensorSimp :: PureTensor -> PureTensor -> Maybe PureTensor
pureTensorSimp pt1@(PT z1 v1) pt2@(PT z2 v2) =
  let isEq = V.zipWith (~=) v1 v2
  in case countFalse isEq of
      0 ->
        Just $ PT (z1 + z2) v1
      1 -> do
        firstFalseIndex <- V.findIndex not isEq
        quotient <- (pt2 ! firstFalseIndex) /^ (pt1 ! firstFalseIndex)
        Just $ PT (z1 + quotient * z2) v1
      _ -> Nothing

-- Utility Functions

countFalse :: V.Vector Bool -> Int
countFalse = V.length . V.filter not

fixpoint :: ApproxEq a => (a -> a) -> a -> a
fixpoint f x =
    let x' = f x
    in if x' ~= x then x else f x'

deleteAtTwo :: (Int, Int) -> [a] -> [a]
deleteAtTwo (i, j) xs = [x | (k, x) <- zip [0..] xs, k /= i, k /= j]

-- pretty print

pp :: Tensor -> IO ()
pp [] = putStrLn ""
pp [pt] = putStrLn $ ppPT pt
pp (pt: pt' : pts) = do
  putStr $ ppPT pt
  putStrLn "+"
  pp (pt' : pts)

ppPT :: PureTensor -> String
ppPT (PT z v) = show z ++ "*" ++ V.foldl1 (\acc str -> acc ++ "⊗" ++ str) (V.map show v)

--
evalByParts :: Int -> Program -> Tensor -> IO Tensor
evalByParts _ [] t = pure t
evalByParts n prog t = do
    let prog1 = take n prog
    let t' = evalProgram prog1 t
    putStrLn $ show (length t') ++ ","
    -- pp t'
    evalByParts n (drop n prog) t'