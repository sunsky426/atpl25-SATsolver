module GenEval.Validation where

import GenEval.AST
--import GenEval.Generator
--import Data.Maybe
import GenEval.Gates
import Data.List
--import GenEval.ANF
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

import Prelude hiding (exp)

-- A simple, 4-qubit evaluation module, for validating a pipeline.
data Ket
  = Zero
  | One 
  | Plus
  | Minus
  deriving (Show,Eq,Ord)

type PureTensor = V.Vector Ket
type TensorTerm = (Double,PureTensor)
type TensorSum  = [TensorTerm]

zeroTensor :: Int -> TensorTerm
zeroTensor n = (1,V.replicate n Zero)

applySingle :: Op -> Int -> TensorTerm -> TensorTerm
applySingle I _ tt = tt
applySingle X i (amp,pt) =
  case pt V.! i of
    Zero -> (amp,V.modify (\v -> MV.write v i One) pt)
    One -> (amp,V.modify (\v -> MV.write v i Zero) pt)
    Plus -> (amp,pt)
    Minus -> (-amp,pt)
applySingle H i (amp,pt) =
  case pt V.! i of
    Zero -> (amp,V.modify (\v -> MV.write v i Plus) pt)
    One -> (amp,V.modify (\v -> MV.write v i Minus) pt)
    Plus -> (amp,V.modify (\v -> MV.write v i Zero) pt)
    Minus -> (amp,V.modify (\v -> MV.write v i One) pt)
applySingle Z i (amp,pt) =
  case pt V.! i of
    Zero -> (amp,pt)
    One -> (-amp,pt)
    Plus -> (amp,V.modify (\v -> MV.write v i Minus) pt)
    Minus -> (amp,V.modify (\v -> MV.write v i Plus) pt)
applySingle Y _ _ = undefined


evalGate :: Gate -> TensorSum -> TensorSum
evalGate (Sing op is) ts =
  foldl' (\tsum i -> map (applySingle op i) tsum) ts (S.toList is)
evalGate (Ctrl op cis i) ts =
  simp $ concatMap gcb ts
  where 
    gcb (amp,pt) =
      let cis' = S.toList cis
          ckets = map (pt V.!) cis'
       in case (any (== Zero) ckets, all (== One) ckets) of
            (True,_) -> [(amp,pt)]
            (_,True) -> [applySingle op i (amp,pt)]
            _ -> 
              let namp = -- TODO: you can use proposition 1 HERE. Remember to force 1
                    amp * foldl 
                      (\acc ket -> 
                        acc * ((case ket of
                          One -> 1
                          Zero -> 0
                          Plus -> 1 / sqrt 2
                          Minus -> - 1 / sqrt 2) :: Double)
                      ) 1.0 ckets
                  npt = foldl' (\tt ind -> V.modify (\v -> MV.write v ind One) tt) pt cis'
                  --npt' = V.modify (\v -> MV.write v i One) npt
                  npt' = applySingle op i (namp,npt)
               in [(amp,pt),npt']


simp :: TensorSum -> TensorSum
simp ts =
  let m = foldl (\acc (amp, pt) ->
                  M.insertWith (+) pt amp acc
                ) M.empty ts
   in [(amp, pt) | (pt, amp) <- M.toList m, not (amp > 0-eps && amp < 0+eps)]
  where eps = 0.00001


evalProgram :: Circuit -> Int -> TensorSum
evalProgram circ n = foldl (\acc x -> evalGate x acc) [zeroTensor n] circ


-- StateVector reading logic

ketAmp :: Ket -> (Double, Double)
ketAmp Zero  = (1, 0)
ketAmp One   = (0, 1)
ketAmp Plus  = (1 / sqrt 2,  1 / sqrt 2)
ketAmp Minus = (1 / sqrt 2, -1 / sqrt 2)

-- amplitude of a tensor term for a specific basis state
ampForBasis :: PureTensor -> Int -> Double
ampForBasis pt basisIndex =
  V.ifoldl' (\acc i ket ->
      let bit = (basisIndex `div` (2^(V.length pt - i - 1))) `mod` 2
          (a0, a1) = ketAmp ket
       in acc * (if bit == 0 then a0 else a1)
    ) 1 pt

-- Expand whole TensorSum into a standard statevector
toStateVector :: Int -> TensorSum -> [Double]
toStateVector n ts =
  [ sum [ amp * ampForBasis pt i | (amp, pt) <- ts ]
  | i <- [0 .. 2^n - 1]
  ]

mostLikelyOutcome :: Int -> TensorSum -> Int
mostLikelyOutcome n ts =
  let sv = toStateVector n ts
      probs = zip [0..] (map (\a -> a*a) sv)
   in fst $ foldl1 (\a@(_,pa) b@(_,pb) -> if pb > pa then b else a) probs

leastLikelyOutcome :: Int -> TensorSum -> Int
leastLikelyOutcome n ts =
  let sv = toStateVector n ts
      probs = zip [0..] (map (\a -> a*a) sv)
   in fst $ foldl1 (\a@(_,pa) b@(_,pb) -> if pb < pa then b else a) probs

formatBits :: Int -> Int -> String
formatBits n i =
  let s = reverse (take n (map (\b -> if b == 1 then '1' else '0') (iterate (`div`2) i)))
   in s

mostLikelyBits :: Int -> TensorSum -> Solution
mostLikelyBits n ts = intToBools n $ mostLikelyOutcome n ts

leastLikelyBits :: Int -> TensorSum -> Solution
leastLikelyBits n ts = intToBools n $ leastLikelyOutcome n ts

intToBools :: Int -> Int -> Solution
intToBools n x =
  [ testBit x (n - 1 - i) | i <- [0 .. n-1] ]

testBit :: Int -> Int -> Bool
testBit x i = (x `div` (2^i)) `mod` 2 == 1


-- Validation logic
type Solution = [Bool]

validate :: Exp -> Solution -> Bool
validate bexp ass =
  case bexp of
    AND e1 e2 -> validateBoth e1 e2 (&&)
    OR e1 e2  -> validateBoth e1 e2 (||)
    XOR e1 e2 -> validateBoth e1 e2 (/=)
    NEG e     -> not (validate e ass)
    Var index -> ass !! index
    Const bool-> bool
  where
    validateBoth e1 e2 f = f (validate e1 ass) (validate e2 ass)
