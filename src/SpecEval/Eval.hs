{-# OPTIONS_GHC -Wno-orphans #-}
module SpecEval.Eval where

import SpecEval.Gates
import qualified Data.Vector as V
import Data.Maybe(isJust)
import Data.List(find)

--- type definations ---
data Qubit =
  Ket0 | Ket1 | KetPlus | KetMinus
  deriving(Eq)

instance Show Qubit where
  show Ket0 = "|0>"
  show Ket1 = "|1>"
  show KetPlus = "|+>"
  show KetMinus = "|->"

qsnd :: Qubit -> Double
qsnd Ket0 = 0
qsnd Ket1 = 1
qsnd KetPlus = sqrt 2 / 2
qsnd KetMinus = - (sqrt 2 / 2)

data PureTensor = PT {scalar :: Double, qbs :: V.Vector Qubit}
  deriving(Show)

-- utility functions for pure tensors
-- updates entries using [(index, updateFunction)]
(//) :: PureTensor -> [(Int, Qubit)] -> PureTensor
(PT z vec) // updates = PT z $ vec V.// updates

-- indexes an entry
(!) :: PureTensor -> Int -> Qubit
(PT _ vec) ! i = vec V.! i

-- multiples by scalar
(*^) :: Double -> PureTensor -> PureTensor
alpha *^ (PT z vec) = PT (alpha * z) vec

type Tensor = [PureTensor]

-- |0..0> in tensor notation
zero :: Int -> Tensor
zero dim = [PT 1 $ V.replicate dim Ket0]

hadamard :: Int -> Tensor
hadamard dim = evalProgram (pow H dim) (zero dim)

--- evaluation functions ---

-- apply all gates in a program sequentiall (via fold)
evalProgram :: Program -> Tensor -> Tensor
evalProgram program tensor = foldl (flip evalGate) tensor program

-- distributes a gate over addition to all pure tensors in a tensor
evalGate :: Gate -> Tensor -> Tensor
evalGate gate = tensorSimp . concatMap (evalTerm gate)

-- evaluates a gate on a pure tensor, using LinAlg Module
evalTerm :: Gate -> PureTensor -> Tensor
evalTerm (Only pos gate) qbs =
  let (z, v) = evalSingle gate (qbs ! pos)
  in pure $ (z *^ qbs) // [(pos, v)]
evalTerm (MCZ pos) qbs =
  case product $ [qsnd (qbs ! i) | i <- pos] of
    0 -> [qbs] -- if beta = 0, there is no correction
    1 -> [(-1) *^ qbs]
    beta -> [qbs, correction] -- calculate correction for non-zero beta
      where
        updates = repeat Ket1
        correction = ((-2 * beta) *^ qbs) // zip pos updates

evalSingle :: SingleGate -> Qubit -> (Double, Qubit)
evalSingle X Ket0 = (1, Ket1)
evalSingle X Ket1 = (1, Ket0)
evalSingle X KetPlus = (1, KetPlus)
evalSingle X KetMinus = (-1, KetMinus)
evalSingle H Ket0 = (1, KetPlus)
evalSingle H Ket1 = (1, KetMinus)
evalSingle H KetPlus = (1, Ket0)
evalSingle H KetMinus = (1, Ket1)

--- simplification functions ---

tensorSimp :: Tensor -> Tensor
tensorSimp tensor =
  let
    size = length tensor
    simpUpdates = [(pureTensorSimp (tensor !! i) (tensor !! j), i, j) | i <- [0..size - 1], j <- [i+1..size-1]]
    -- the list is [result, i, j] where i, j are every pair of puretensors
    --  and result is what the two tensors can be simplified to (or Nothing if they can't be similfied)
    firstSimp = find (\(a, _, _) -> isJust a) simpUpdates -- find the first available simplication
  in
    case firstSimp of
      Nothing -> tensor -- if there is no simplification available, return the tensor
      Just simp ->
        case simp of
          (Just pt', i, j) -> tensorSimp (pt' : deleteAtTwo (i, j) tensor) -- if successfully applied simp rule, look for more simplications
          (Nothing, _, _) -> tensor -- this case shouldn't be possible

pureTensorSimp :: PureTensor -> PureTensor -> Maybe PureTensor
pureTensorSimp _pt1@(PT z1 v1) _pt2@(PT z2 v2) =
  let isEq = V.zipWith (==) v1 v2
  in case countFalse isEq of
      0 ->
        Just $ PT (z1 + z2) v1
        -- if the puretensors are equal up to a scalar, then just sum the scalars (rule 3)
      -- 1 -> do
      --   firstFalseIndex <- V.findIndex not isEq
      --   let (q1, q2) = (v1 V.! firstFalseIndex, v2 V.! firstFalseIndex)
      --   Just $ PT 1 (v1 V.// [(firstFalseIndex, z1 LA.*^ q1 + z2 LA.*^ q2)])
        -- if the puretensors are equal up to a scalar except for one entry k. then combine the entry using z1 * v1_i + z2 + v2_i 
      _ -> Nothing

clearZero :: Tensor -> Tensor
clearZero = filter (\(PT z _) -> not (z ~= 0))

--- Utility Functions ---

(~=) :: Double -> Double -> Bool
a ~= b = abs (a - b) <= 1e-9

countFalse :: V.Vector Bool -> Int
countFalse = V.length . V.filter not

-- delete two elements of a list by index
deleteAtTwo :: (Int, Int) -> [a] -> [a]
deleteAtTwo (i, j) xs = [x | (k, x) <- zip [0..] xs, k /= i, k /= j]

-- evaluate Program by parts so I see what happens when it stalls
evalByParts :: Int -> Program -> Tensor -> IO Tensor
evalByParts _ [] t = pure t
evalByParts n prog t = do
    let prog1 = take n prog
    let t' = evalProgram prog1 t
    putStrLn $ show (length t') ++ "," ++ show (last prog1)
    pp t'
    evalByParts n (drop n prog) t'

scanProgram :: Program -> Tensor -> [Int]
scanProgram program tensor = map length $ scanl (flip evalGate) tensor program

--- pretty printer ---

pp :: Tensor -> IO ()
pp [] = putStrLn ""
pp [pt] = putStrLn $ ppPT pt
pp (pt: pt' : pts) = do
  putStr $ ppPT pt
  putStrLn "+"
  pp (pt' : pts)

ppPT :: PureTensor -> String
ppPT (PT z v) = show z ++ " * " ++ V.foldl1 (\acc str -> acc ++ " ⊗ " ++ str) (V.map show v)
