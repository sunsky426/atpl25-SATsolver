module PrettyPrinter where

import ANF
import Tensors
import Data.Complex
import qualified Data.Vector as V

-- mostly for debugging/inspecting parts of a (sub)computation.

ppAnf :: ANF -> String
ppAnf anf = 
  case anf of
    Cst b -> show b
    Pos i -> show i
    Xor e1 e2 -> '(' : ppAnf e1 ++ ")" ++ " ^ " ++ "(" ++ ppAnf e2 ++ ")"
    And e1 e2 -> ppAnf e1 ++ " & " ++ ppAnf e2

ppAmplitude :: Amplitude -> String
ppAmplitude amp =
  case (realPart amp, imagPart amp) of
    (r,0) -> show r
    (0,i) -> show i ++ "i"
    (r,i) -> 
      let sign = if i < 0 then "-" else "+"
       in '(' : show r ++ " " ++ sign ++ " " ++ show (i * signum i) ++ "i)"

ppQubit :: Qubit -> String
ppQubit (q1,q2) = '(' : ppAmplitude q1 ++ ", " ++ ppAmplitude q2 ++ ")"

ppPureTensor :: PureTensor -> String
ppPureTensor pt = unwords $ (map ppQubit (V.toList pt))

ppTensorTerm :: TensorTerm -> String
ppTensorTerm (amp,pt) = ppAmplitude amp ++ " " ++ ppPureTensor pt

ppTensorSum :: TensorSum -> String
ppTensorSum [] = ""
ppTensorSum (tt:rest) = ppTensorTerm tt ++ "\n" ++ ppTensorSum rest
