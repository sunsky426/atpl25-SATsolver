module SpecEval.Measure where

import SpecEval.Eval ( Tensor, PureTensor(..), Qubit(..), (~=))
import Numeric.LinearAlgebra
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import Data.List (findIndices)

--- measurement ---

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

greedyMeasure :: Vector Double -> Int
greedyMeasure v = VS.maxIndex $ VS.map (\x -> x*x) v

--- cheaty inspection ---
seperateSolution :: Vector Double -> ([Int], [Int])
seperateSolution v =
  let 
    l = VS.toList v
    mu = sum l / fromIntegral (length l)
  in case findIndices (~= mu) l of
    [] -> (findIndices (>= mu) l, findIndices (< mu) l)
    s -> (s, [])