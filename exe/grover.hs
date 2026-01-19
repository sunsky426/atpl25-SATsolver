{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{- HLINT ignore "Use lambda-case" -}
import GenEval as GE
import SpecEval as SE
import Data.Vector as V
import Data.List as L
import System.Environment
import Criterion.Main
import Numeric.LinearAlgebra as NL
import qualified Data.Set as S

getBestSE :: [SE.PureTensor] -> String
getBestSE (_ : SE.PT _ qbs : _) =
  let l = V.toList qbs in
    L.map ((\qb -> case qb of
                    [amp0, amp1] | abs amp0 > abs amp1 -> '0'
                    [_, _] -> '1'
                    _ -> error "Expected qubit to have two elements") . NL.toList . SE.unQubit) l
getBestSE _ = error "Get best needs two pure tensors"

simpleOracleSE :: Int -> [SE.Gate]
simpleOracleSE qbCount = [MCZ [0..qbCount-1]]

overlapOracleSE :: Int -> [SE.Gate]
overlapOracleSE qbCount = [
    MCZ [0..qbn]
    | qbn <- [1 .. qbCount-1]
  ]

complexOracleSE :: Int -> [SE.Gate]
complexOracleSE qbCount = [
    MCZ [qbn..qbn+1]
    | qbn <- [0 .. qbCount-2]
  ]  

runGroversSE :: [SE.Gate] -> Int -> Tensor
runGroversSE oracle qbCount = 
  let groversCircuit = grovers qbCount oracle (numberOfRuns qbCount) 
  in SE.evalProgram groversCircuit (zero qbCount)

simpleOracleGE :: Int -> [GE.Gate]
simpleOracleGE qbCount = singleSol qbCount [] 

overlapOracleGE :: Int -> [GE.Gate]
overlapOracleGE qbCount = [
    Ctrl Z (S.fromList [1 .. qbn]) 0
    | qbn <- [1 .. qbCount-1]
  ]

complexOracleGE :: Int -> [GE.Gate]
complexOracleGE qbCount = [
    Ctrl Z (S.fromList [qbn]) (qbn+1)
    | qbn <- [0 .. qbCount-2] 
  ]

runGroversGE :: Integral t => [GE.Gate] -> t -> [PureTensorIV]
runGroversGE oracle qbCount =
  let groversCircuit = generateGroversCircuit oracle (fromIntegral qbCount)
  in  eval groversCircuit 1 [qubit (sqrt 1) (sqrt 0) | _ <- [0 .. (qbCount-1)]]
    
main :: IO ()
main = do
    args <- getArgs
    case args of
        "bench-simp-spec":qbCountStr:_ -> do
          let maxQbCount = read qbCountStr
              benchList = [
                  bench (show qbCount L.++ " qubits") $ nf (getBestSE . runGroversSE (simpleOracleSE qbCount)) qbCount
                  | qbCount <- [2 .. maxQbCount]
                ]
          withArgs (L.drop 2 args) $ defaultMain [
              bgroup "Grovers with single control-Z gate oracle using specialized evaluator" benchList
            ]
        "bench-overlap-spec":qbCountStr:_ -> do
          let maxQbCount = read qbCountStr
              benchList = [
                  bench (show qbCount L.++ " qubits") $ nf (getBestSE . runGroversSE (overlapOracleSE qbCount)) qbCount
                  | qbCount <- [2 .. maxQbCount]
                ]
          withArgs (L.drop 2 args) $ defaultMain [
              bgroup "Grovers with overlapping control gates oracle using specialized evaluator" benchList
            ]
        "bench-complex-spec":qbCountStr:_ -> do
          let maxQbCount = read qbCountStr
              benchList = [
                  bench (show qbCount L.++ " qubits") $ nf (getBestSE . runGroversSE (complexOracleSE qbCount)) qbCount
                  | qbCount <- [2 .. maxQbCount]
                ]
          withArgs (L.drop 2 args) $ defaultMain [
              bgroup "Grovers with complex oracle oracle using specialized evaluator" benchList
            ]
        "bench-simp-gen":qbCountStr:_ -> do
          let maxQbCount = read qbCountStr
              benchList = [
                  bench (show qbCount L.++ " qubits") $ nf (getBest . runGroversGE (simpleOracleGE qbCount)) qbCount
                  | qbCount <- [2 .. maxQbCount]
                ]
          withArgs (L.drop 2 args) $ defaultMain [
              bgroup "Grovers with single control-Z gate oracle using general evaluator" benchList
            ]
        "bench-overlap-gen":qbCountStr:_ -> do
          let maxQbCount = read qbCountStr
              benchList = [
                  bench (show qbCount L.++ " qubits") $ nf (getBest . runGroversGE (overlapOracleGE qbCount)) qbCount
                  | qbCount <- [2 .. maxQbCount]
                ]
          withArgs (L.drop 2 args) $ defaultMain [
              bgroup "Grovers with overlapping control gates oracle using general evaluator" benchList
            ]
        "bench-complex-gen":qbCountStr:_ -> do
          let maxQbCount = read qbCountStr
              benchList = [
                  bench (show qbCount L.++ " qubits") $ nf (getBest . runGroversGE (complexOracleGE qbCount)) qbCount
                  | qbCount <- [2 .. maxQbCount]
                ]
          withArgs (L.drop 2 args) $ defaultMain [
              bgroup "Grovers with complex oracle using general evaluator" benchList
            ]
        "spec-sv":qbCountStr:negListStr -> do
          let qbCount = read qbCountStr
              nl = parseNegList negListStr qbCount
              oracle = [Only qp SE.X | qp <- nl] L.++ [MCZ [0..qbCount-1]] L.++ [Only qp SE.X | qp <- nl]
              groversCircuit = grovers qbCount oracle (numberOfRuns qbCount)
              solution = vectorize $ SE.evalProgram groversCircuit (zero qbCount)
          putStr $ ppSV solution
        "spec-one":qbCountStr:negListStr -> do
          let qbCount = read qbCountStr
              nl = parseNegList negListStr qbCount
              oracle = [Only qp SE.X | qp <- nl] L.++ [MCZ [0..qbCount-1]] L.++ [Only qp SE.X | qp <- nl]
              groversCircuit = grovers qbCount oracle (numberOfRuns qbCount)
              solution = getBestSE $ SE.evalProgram groversCircuit (zero qbCount)
          putStr solution
        "gen-sv":qbCountStr:negListStr -> do
          let qbCount = read qbCountStr
              groversResult = runGroversSingleSol qbCount (parseNegList negListStr qbCount)
          putStr $ ppSV $ tensorToStateVector groversResult
        "gen-one":qbCountStr:negListStr -> do
          let qbCount = read qbCountStr
              groversResult = runGroversSingleSol qbCount (parseNegList negListStr qbCount)
          putStr $ getBest groversResult
          putStr "\n"
        _ -> do
            putStrLn "Usage:"
            putStrLn "Running benchmarks up to some number of qubits using specified oracle and evaluator: "
            putStrLn "\tbench-<simp|complex|overlap>-<gen|spec> <number of qubits>"
            putStrLn "\tExample: \"cabal run grover -- bench-simp-spec 10\""
            putStrLn ""
            putStrLn "Running grover single solution using specified evaluator: "
            putStrLn "\tOutput only correct solution:"
            putStrLn "\t<spec|gen>-one <number of qubits> [qubits to negate]"
            putStrLn "\tExample: \"cabal run grover -- 4 0,1\""
            putStrLn ""
            putStrLn "\tOutput statevector:"
            putStrLn "\t<spec|gen>-sv <number of qubits> [qubits to negate]"
            putStrLn "\tExample: \"cabal run grover -- sv 4\""