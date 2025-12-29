module Eval where

import Comp
import Data.Complex
import Control.Monad.ST
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

type Amplitude   = Complex Double
type Qubit       = (Amplitude,Amplitude)
type PureTensor  = (Amplitude,V.Vector Qubit)
type TensorTerm  = [PureTensor]

eps :: Amplitude
eps = 0.00001 :+ 0.0001

(~=) :: Amplitude -> Amplitude -> Bool
a1 ~= a2 = magnitude (a1 - a2) <= magnitude eps

i :: Amplitude
i = 0 :+ 1

zero :: Amplitude
zero = 0 :+ 0

one :: Amplitude
one = 1 :+ 0

evalOp :: Op -> Qubit -> Qubit
evalOp I (a0,a1) = (a0,a1)  -- Identity.
evalOp X (a0,a1) = (a1,a0)  -- X swaps amplitudes.
evalOp Z (a0,a1) = (a0,-a1) -- Z flips sign of |1>.
evalOp Y (a0,a1) = (-i * a1, i * a0)  -- Y does Y things.
evalOp H (a0,a1) =  
  ((a0 + a1) / sqrt 2,
   (a0 - a1) / sqrt 2)

baseZero :: Int -> ST s (MV.MVector s Qubit)
baseZero size = do
  MV.replicate size (zero,zero)

evalQP :: QP -> Int -> V.Vector Qubit
evalQP qp size = runST $ do
  v1 <- baseZero size
  v2 <- baseZero size
  V.freeze

--evalGate :: Gate -> [MV.MVector s Qubit] -> ST s ()
--evalGate (Single op pos) vec = do
--  MV.modify vec (\q -> evalOp op q) pos
--evalGate (C idx op pos) vec =
--  case idx of
--    (i:[]) -> do 
--      (ca,cb) <- MV.read vec i
--      case (ca ~= zero,cb ~= zero) of
--        (True,_) -> pure ()
--        (_,True) -> MV.modify vec (\q -> evalOp op q) pos
--        _        -> undefined
--    (i:ii) -> undefined
