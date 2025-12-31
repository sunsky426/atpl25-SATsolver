module Eval where

import Gates
import Data.Complex
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

type Amplitude  = Complex Double
type Qubit      = (Amplitude,Amplitude)
type PureTensor = Vector Qubit
type TensorTerm = (Amplitude,PureTensor)
type TensorSum  = [TensorTerm]

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

zeroTensor :: Int -> TensorTerm
zeroTensor n = (one,V.replicate n (one,zero))

-- Quickly deprecate this, as some effects can be retracted from the qubits into the scalar (e.g. Z).
evalOp :: Op -> Qubit -> Qubit
evalOp I (a0,a1) = (a0,a1)  -- Identity.
evalOp X (a0,a1) = (a1,a0)  -- X swaps amplitudes.
evalOp Z (a0,a1) = (a0,if a1 ~= zero then a1 else -a1) -- Z flips sign of |1>.
evalOp Y (a0,a1) = (-i * a1, i * a0)  -- Y does Y things.
evalOp H (a0,a1) =  
  ((a0 + a1) / sqrt 2,
   (a0 - a1) / sqrt 2)

evalGate :: Gate -> TensorSum -> TensorSum
evalGate (Single op pos) tsum = 
  map (\(amp,vec) -> 
    (amp,V.modify (\v -> MV.modify v (\v' -> evalOp op v') pos) vec)
  ) tsum
evalGate (CZ pos) tsum =
  concatMap applyCZTerm tsum
  where
    applyCZTerm :: TensorTerm -> TensorSum
    applyCZTerm (amp, vec) =
      let
        -- extract |1> amplitudes on control qubits
        oneAmps :: [Amplitude]
        oneAmps = [ snd (vec V.! p) | p <- pos ]

        factor :: Amplitude
        factor = product oneAmps
      in
        if factor ~= zero
        then
          [ (amp, vec)
          , (-2 * amp * factor, forceOnes vec)
          ]
        else
          [ (amp, vec) ]

    forceOnes :: PureTensor -> PureTensor
    forceOnes =
      V.modify $ \v ->
        mapM_ (\p -> MV.write v p (zero, one)) pos
evalGate (C _ _ _) _ = undefined -- unused for now.

evalProgram :: QP -> Int -> TensorSum
evalProgram qp n = foldl (flip evalGate) [(zeroTensor n)] qp

