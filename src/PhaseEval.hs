module PhaseEval where

import Gates
import Tensors
import Data.Complex
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

eps :: Amplitude
eps = 0.00001 :+ 0.00001

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

evalOp :: Op -> Qubit -> Qubit
evalOp I (a0,a1) = (a0,a1)  -- Identity.
evalOp X (a0,a1) = (a1,a0)  -- X swaps amplitudes.
evalOp Z (a0,a1) = (a0,-a1) -- Z flips sign of |1>.
evalOp Y (a0,a1) = (-i * a1, i * a0)  -- Y does Y things.
evalOp H (a0,a1) =  
  ((a0 + a1) / sqrt 2,
   (a0 - a1) / sqrt 2)

evalGate :: QGate -> TensorSum -> TensorSum
evalGate (Single op pos) tsum = 
  map (\(amp,vec) -> 
    (amp,V.modify (\v -> MV.modify v (\v' -> evalOp op v') pos) vec)) tsum
evalGate (CZ pos) tsum = 
--  where 
--    applyCZ :: TensorTerm -> TensorSum
--    applyCZ (amp,vec) =
--      let newAmp = (foldl (\y x -> (snd (vec V.! x)) * y) one pos)
--       in if newAmp ~= zero
--            then [(amp,vec)] -- some control-gate was zero
--            else [(amp,vec),
--                  (newAmp,V.modify (\v -> MV.write v one pos) vec)]
--concatMap applyCZ tsum
--where
--  applyCZ (amp,vec) =
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
evalGate (C _ _ _) _ = undefined -- unused in this interpretation.

evalProgram :: QP -> Int -> TensorSum
evalProgram qp n = foldl (flip evalGate) [(zeroTensor n)] qp
