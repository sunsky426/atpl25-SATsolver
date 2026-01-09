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

data Gate =
    Sing  Op (Set Int)
  | Ctrl  Op (Set Int) Integer
  deriving Show

type Circuit = [Gate]

-- type (PrimMonad s) => Tensor s = [PureTensor s]

-- cmp a b = a < b

-- type (PrimMonad s) => V.MVector s

data PureTensorMV s = PT {
  scalar :: C,
  qbs :: V.MVector (PrimState (ST s)) Qubit
}

data PureTensorIV = PTIV {
  scalarIV :: C,
  qbsIV :: V.Vector Qubit
} deriving Show

type TensorMV s = [PureTensorMV s]

-- createInt :: C -> [Qubit] -> ST s Int
-- createInt scalar l = do
--   v <- V.thaw $ V.fromList l
--   return 0

createPT :: C -> [Qubit] -> ST s (PureTensorMV s)
createPT scalar l = do
  v <- V.thaw $ V.fromList l
  return $ PT scalar v

-- applySingle sg (PT s qbs) =

-- applySingle op = 

toIV :: PureTensorMV s -> ST s PureTensorIV
toIV (PT s qbs) = do
  v <- V.freeze qbs
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
      tqb <- VM.read qbs (fromInteger target)
      l <- Prelude.mapM (VM.read qbs) $ S.toList control_set
      let res = evalSingle op tqb
      if res ~= tqb || (~= 1) (L.foldl (\acc q -> acc * qfst q) 1 l) then return Nothing
      else
        case L.foldl (\acc q -> acc * qsnd q) 1 l of
        beta | beta ~= 1 -> do
          VM.modify qbs (evalSingle op) (fromInteger target)
          return Nothing
        beta -> do
          newPT <- VM.clone qbs
          VM.modify newPT (\q -> evalSingle op q - q) (fromInteger target)
          Prelude.mapM_ (VM.modify newPT (setQubit (const 0) (const 1))) control_set
          return $ Just $ PT (alph *beta) newPT

simpPureTensorQ :: PureTensorMV s -> PureTensorMV s -> ST s (Maybe C)
simpPureTensorQ (PT _ qbs1) (PT s2 qbs2) = do
    v1 <- V.freeze qbs1
    v2 <- V.freeze qbs2 -- find more optimale
    case V.toList $ V.findIndices id $ V.zipWith (\x y -> not (x ~= y)) v1 v2 of
      -- simplify if puretensors are equal up to scalar
      [] -> return $ Just s2
      -- simplify if equal up to a scalar except one factor
      -- [i] -> do
      --   val1 <- VM.read qbs1 i
      --   val2 <- VM.read qbs2 i
      --   return $ (* s2) <$> val1 /^ val2
      _ -> return Nothing

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

