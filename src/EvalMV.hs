{- HLINT ignore "Use tuple-section" -}
module EvalMV where

import Control.Monad.ST

import LinAlg
import Data.Vector.Mutable as VM
import Data.Set as S
import Data.List as L
import Data.Vector as V hiding (catMaybes)
import Gates
import Eval (Tensor)
import qualified GHC.Real as L
import Data.Maybe
import Numeric.LinearAlgebra
import Debug.Trace

data Gate =
    Sing  Op (Set Int)
  | Ctrl  Op (Set Int) Int
  deriving Show

type Circuit = [Gate]

data PureTensorMV s = PT {
  scalarMV :: C,
  qbsMV :: V.MVector (PrimState (ST s)) Qubit
}

data PureTensorIV = PTIV {
  scalarIV :: C,
  qbsIV :: V.Vector Qubit
} deriving Show

type TensorMV s = [PureTensorMV s]

createPT :: C -> [Qubit] -> ST s (PureTensorMV s)
createPT scalar l = do
  v <- V.thaw $ V.fromList l
  return $ PT scalar v

toMV :: PureTensorIV -> ST s (PureTensorMV s)
toMV (PTIV s qbs) = do 
  v <- thaw qbs 
  return $ PT {
    scalarMV = s,
    qbsMV = v
  }

toIV :: PureTensorMV s -> ST s PureTensorIV
toIV (PT s qbs) = do
  v <- freeze qbs
  return $ PTIV {
    scalarIV = s,
    qbsIV = v
  }

applyGate :: Gate -> PureTensorMV s -> ST s (Maybe (PureTensorMV s))
applyGate gate pt@(PT alph qbs) =
  case gate of
    Sing op target_set -> do
      Prelude.mapM_ (VM.modify qbs (evalSingle op)) target_set
      return Nothing
    Ctrl op control_set target -> do
      tqb <- VM.read qbs target --(fromInteger target)
      l <- Prelude.mapM (VM.read qbs) $ S.toList control_set
      let res = evalSingle op tqb
      if res ~= tqb || (~= 1) (L.foldl (\acc q -> acc * qfst q) 1 l) then return Nothing
      else
        case L.foldl (\acc q -> acc * qsnd q) 1 l of
        beta | beta ~= 1 -> do
          VM.modify qbs (evalSingle op) target --(fromInteger target)
          return Nothing
        beta -> do
          newPT <- VM.clone qbs
          tq <- VM.read newPT target
          let diff = evalSingle op tq - tq
              gamma = sqrt $ dot (unQubit diff) (unQubit diff)
          VM.write newPT target (qubit (qfst diff / gamma) (qsnd diff / gamma)) 
          Prelude.mapM_ (VM.modify newPT (setQubit (const 0) (const 1))) control_set
          return $ Just $ PT (alph * beta * gamma) newPT

simpPureTensorQ :: PureTensorMV s -> PureTensorMV s -> ST s (Maybe C)
simpPureTensorQ (PT _ qbs1) (PT s2 qbs2) = do

    l <- find 0 []

    case l of
      -- simplify if puretensors are equal up to scalar
      Just [] -> return $ Just s2
      -- simplify if equal up to a scalar except one factor
      Just [val1, val2] -> do
        return $ (* s2) <$> val2 /^ val1
      _ -> return Nothing

  where find i res =
          if i == VM.length qbs1 then return (Just res)
          else do
            val1 <- VM.read qbs1 i
            val2 <- VM.read qbs2 i
            if not (val1 ~= val2) then
              if L.null res then do
                find (i+1) [val1, val2]
              else
                return Nothing
            else find (i+1) res

simpTensor :: TensorMV s -> ST s (TensorMV s)
simpTensor [] = return []
simpTensor (pt@(PT s qbs) : rest) = do
  l <- Prelude.mapM (\q -> do
    r <- simpPureTensorQ pt q
    return (r, q)) rest
  let notSimpList = L.map snd $ L.filter (isNothing . fst) l
      simpQ = Data.Maybe.mapMaybe fst l
  res <- simpTensor notSimpList
  return $ PT (L.foldl (+) s simpQ) qbs : res


evalCircuit :: Circuit -> TensorMV s -> ST s (TensorMV s)
evalCircuit [] tensor = simpTensor tensor
evalCircuit (h : t) tensor = do
  l <- Prelude.mapM (applyGate h) tensor
  s <- simpTensor (tensor L.++ catMaybes l)
  evalCircuit t s

eval :: [Gate] -> C -> [Qubit] -> [PureTensorIV]
eval circuit scalar qbs = runST evaluator
  where evaluator = do
          q <- V.thaw $ V.fromList qbs
          res <- evalCircuit circuit [PT scalar q]
          Prelude.mapM toIV res


