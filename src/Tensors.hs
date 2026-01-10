module Tensors where

import Data.Vector
import Data.Complex

type Amplitude  = Complex Double
type Qubit      = (Amplitude,Amplitude)
type PureTensor = Vector Qubit
type TensorTerm = (Amplitude,PureTensor)
type TensorSum  = [TensorTerm]
