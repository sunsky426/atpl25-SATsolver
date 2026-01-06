module Measure where

import Eval ( Tensor, PureTensor(..), Qubit(..), (~=))
import Numeric.LinearAlgebra
import qualified Data.Vector as V

unQubit :: Qubit -> Vector Double
unQubit Ket0 = fromList [1, 0]
unQubit Ket1 = fromList [0, 1]
unQubit KetPlus = fromList [sqrt 2 / 2, sqrt 2 / 2]
unQubit KetMinus = fromList [sqrt 2 / 2, - (sqrt 2 / 2)]

vectorize :: Tensor -> Vector Double
vectorize t = cmap roundZero $ sum $ vectorizePT <$> t

vectorizePT :: PureTensor -> Vector Double
vectorizePT (PT z v) = scale z $ V.foldl1 f (unQubit <$> v)
  where f x y = flatten $ outer x y

roundZero :: Double -> Double
roundZero x = if x ~= 0 then 0 else x