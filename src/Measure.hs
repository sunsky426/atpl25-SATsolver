module Measure where

import Eval ( Tensor, PureTensor(..) )
import Numeric.LinearAlgebra
import LinAlg(unQubit)
import qualified Data.Vector as V

vectorize :: Tensor -> Vector C
vectorize t = sum $ vectorizePT <$> t

vectorizePT :: PureTensor -> Vector C 
vectorizePT (PT z v) = scale z $ V.foldl1 f (unQubit <$> v)
  where f x y = flatten $ outer x y