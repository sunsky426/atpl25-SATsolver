module GenEval.Grovers where

import GenEval.Gates
import GenEval.Eval
import Data.Set as S
import Data.List as L
import GenEval.LinAlg


numberOfRuns :: Int -> Int
numberOfRuns qbCount = floor $ ((pi :: Float) / (4 :: Float)) * sqrt (2 ^ qbCount)

singleSol :: Int -> [Int] -> [Gate]
singleSol qbCount [] = [
    Ctrl Z (S.fromList $ tail [0 .. qbCount-1]) 0
  ]
singleSol qbCount negList = [
    Sing X (S.fromList negList),

    Ctrl Z (S.fromList $ tail [0 .. qbCount-1]) 0,

    Sing X (S.fromList negList)
  ]

generateGroversCircuit :: [Gate] -> Int -> [Gate]
generateGroversCircuit oracle qbCount =
   Sing H (S.fromList allQ) :
   concat (replicate (numberOfRuns qbCount) (oracle ++ amplification))
  where allQ = [0 .. qbCount-1]
        amplification = [
            Sing H (S.fromList allQ),
            Sing X (S.fromList allQ),
            Ctrl Z (S.fromList $ tail allQ) 0,
            Sing X (S.fromList allQ),
            Sing H (S.fromList allQ)
          ]

runGroversSingleSol :: Int -> [Int] -> [PureTensorIV]
runGroversSingleSol qbCount negList =
  let oracle = singleSol qbCount negList
      groversCircuit = generateGroversCircuit oracle (fromIntegral qbCount)
  in  eval groversCircuit 1 [qubit (sqrt 1) (sqrt 0) | _ <- [0 .. (qbCount-1)]]

parseNegList :: [String] -> Int -> [Int]
parseNegList negListStr qbCount
  | L.null negListStr = []
  | negListStr == ["all"] = [0 .. qbCount-1]
  | otherwise = read ("[" ++ head negListStr ++ "]") :: [Int]

pow :: Op -> Int -> Circuit
pow op i = L.map (Sing op . S.singleton) [0..i-1]

diffusion :: Int -> Circuit
diffusion n =
  pow H n ++
  pow X n ++
  [Ctrl Z (S.fromList [0..n-2]) (n-1)] ++
  pow X n ++
  pow H n

groverIteration :: Circuit -> Circuit -> Int -> Circuit
groverIteration oracle diffuser 1 = oracle ++ diffuser
groverIteration oracle diffuser n =
  oracle ++ diffuser ++ groverIteration oracle diffuser (n-1)

grover :: Circuit -> Int -> Circuit
grover oracle n =
  pow H n ++ groverIteration oracle (diffusion n) (floor ((pi / 4.0 * sqrt (2 ^ n)) :: Double))
