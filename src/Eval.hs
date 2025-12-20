{-# OPTIONS_GHC -Wno-orphans #-}
module Eval where

import Gates
import LinAlg
import qualified Data.Vector as V
import Macros

-- type definations

data PureTensor = PT {scalar :: CC, qbs :: V.Vector Qubit}
  deriving(Show)

-- utility functions for pure tensors
-- updates entries using [(index, updateFunction)]
(//) :: PureTensor -> [(Int, Qubit -> Qubit)] -> PureTensor
(PT z vec) // updates = PT z $ vec V.// (phi <$> updates)
  where
    phi :: (Int, Qubit -> Qubit) -> (Int, Qubit)
    phi (index, f) = (index, f (vec V.! index))

-- indexes an entry
(!) :: PureTensor -> Int -> Qubit
(PT _ vec) ! i = vec V.! i

-- multiples by scalar
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
  if all (~=0) ([qfst (qbs ! i) | i <- ctrls])
  then [qbs // [(target, evalSingle gate)]] -- if the all the control have qfst q = 0, the just apply G on target
  else case product $ [qsnd (qbs ! i) | i <- ctrls] of
    0 -> [qbs] -- if beta = 0, there is no correction
    beta -> [qbs, correction] -- calculate correction for non-zero beta
      where
        targetUpdate q = evalSingle gate q - q
        ctrlUpdates = repeat $ const $ qubit 0 1
        correction = (beta *^ qbs) // zip (target : ctrls) (targetUpdate : ctrlUpdates)

tensorSimp :: Tensor -> Tensor
tensorSimp tensor = f tensor simpUpdates
  where
    size = length tensor
    simpUpdates = [(pureTensorSimp (tensor !! i) (tensor !! j), i, j) | i <- [0..size - 1], j <- [i+1..size-1]]
    -- the list is [result, i, j] where i, j are every pair of puretensors and result is what the two tensors can be simplified to (or Nothing oif they can't be similfied)
    f :: Tensor -> [(Maybe PureTensor, Int, Int)] -> Tensor
    -- f goes through simpUpdates for a successful rewrite and applies the first one it see, then exits
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
        -- if the puretensors are equal up to a scalar, then just sum the scalars (rule 3)
      1 -> do
        firstFalseIndex <- V.findIndex not isEq
        quotient <- (pt2 ! firstFalseIndex) /^ (pt1 ! firstFalseIndex)
        Just $ PT (z1 + quotient * z2) v1
        -- if the puretensors are equal up to a scalar except for one factor. 
        -- Then we see if the factor in question are equal up to a scalar k. If so, factor k to the front and then sum the scalars.
      _ -> Nothing

-- Utility Functions

countFalse :: V.Vector Bool -> Int
countFalse = V.length . V.filter not

fixpoint :: ApproxEq a => (a -> a) -> a -> a
fixpoint f x =
    let x' = f x
    in if x' ~= x then x else f x'

-- delete two elements of a list by index
deleteAtTwo :: (Int, Int) -> [a] -> [a]
deleteAtTwo (i, j) xs = [x | (k, x) <- zip [0..] xs, k /= i, k /= j]

-- pretty printer
pp :: Tensor -> IO ()
pp [] = putStrLn ""
pp [pt] = putStrLn $ ppPT pt
pp (pt: pt' : pts) = do
  putStr $ ppPT pt
  putStrLn "+"
  pp (pt' : pts)

ppPT :: PureTensor -> String
ppPT (PT z v) = show z ++ "*" ++ V.foldl1 (\acc str -> acc ++ "⊗" ++ str) (V.map show v)

-- evaluate Program by parts so I see what happens when it stalls
evalByParts :: Int -> Program -> Tensor -> IO Tensor
evalByParts _ [] t = pure t
evalByParts n prog t = do
    let prog1 = take n prog
    let t' = evalProgram prog1 t
    putStrLn $ show (length t') ++ ","
    -- pp t'
    evalByParts n (drop n prog) t'