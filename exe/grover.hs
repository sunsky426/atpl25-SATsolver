{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{- HLINT ignore "Use lambda-case" -}
import GenEval as GE
import SpecEval as SE
import Data.Vector as V
import Data.List as L
import System.Environment
import Criterion.Main
import Numeric.LinearAlgebra as NL

getBestSE :: [SE.PureTensor] -> String
getBestSE (_ : SE.PT _ qbs : _) =
  let l = V.toList qbs in
    L.map ((\qb -> case qb of
                    [amp0, amp1] | abs amp0 > abs amp1 -> '0'
                    [_, _] -> '1'
                    _ -> error "Expected qubit to have two elements") . NL.toList . SE.unQubit) l
getBestSE _ = error "Get best needs two pure tensors"

runGroversSE :: Int -> Tensor
runGroversSE qbCount = 
  let oracle = [MCZ [0..qbCount-1]]
      groversCircuit = grovers qbCount oracle (numberOfRuns qbCount) 
  in evalProgram groversCircuit (zero qbCount)
    

main :: IO ()
main = do
    args <- getArgs
    case args of
        "bench-spec":qbCountStr:_ -> do
          let maxQbCount = read qbCountStr
              benchList = [
                  bench (show qbCount L.++ " qubits") $ nf (getBestSE . runGroversSE) qbCount
                  | qbCount <- [2 .. maxQbCount]
                ]
          withArgs (L.drop 2 args) $ defaultMain [
              bgroup "Grovers with single control-Z gate oracle using specialized evaluator" benchList
            ]
        "bench-gen":qbCountStr:_ -> do
          let maxQbCount = read qbCountStr
              benchList = [
                  bench (show qbCount L.++ " qubits") $ nf (getBest . flip runGrovers []) qbCount
                  | qbCount <- [2 .. maxQbCount]
                ]
          withArgs (L.drop 2 args) $ defaultMain [
              bgroup "Grovers with single control-Z gate oracle using general evaluator" benchList
            ]
        "spec-sv":qbCountStr:negListStr -> do
          let qbCount = read qbCountStr
              nl = parseNegList negListStr qbCount
              oracle = [Only qp SE.X | qp <- nl] L.++ [MCZ [0..qbCount-1]] L.++ [Only qp SE.X | qp <- nl]
              groversCircuit = grovers qbCount oracle (numberOfRuns qbCount)
              solution = vectorize $ evalProgram groversCircuit (zero qbCount)
          putStr $ ppSV solution
        "spec-one":qbCountStr:negListStr -> do
          let qbCount = read qbCountStr
              nl = parseNegList negListStr qbCount
              oracle = [Only qp SE.X | qp <- nl] L.++ [MCZ [0..qbCount-1]] L.++ [Only qp SE.X | qp <- nl]
              groversCircuit = grovers qbCount oracle (numberOfRuns qbCount)
              solution = getBestSE $ evalProgram groversCircuit (zero qbCount)
          putStr solution
        "gen-sv":qbCountStr:negListStr -> do
          let qbCount = read qbCountStr
              groversResult = runGrovers qbCount (parseNegList negListStr qbCount)
          putStr $ ppSV $ tensorToStateVector groversResult
        "gen-one":qbCountStr:negListStr -> do
          let qbCount = read qbCountStr
              groversResult = runGrovers qbCount (parseNegList negListStr qbCount)
          putStr $ getBest groversResult
          putStr "\n"
        _ -> do
            putStrLn "Usage:"
            putStrLn "Running benchmarks up to some number of qubits: "
            putStrLn "\t<number of qubits>"
            putStrLn "\tExample: \"cabal run grover -- bench 10\""
            putStrLn ""
            putStrLn "Running grover single solution: "
            putStrLn "\tOutput only correct solution:"
            putStrLn "\t<number of qubits> [qubits to negate]"
            putStrLn "\tExample: \"cabal run grover -- 4 0,1\""
            putStrLn ""
            putStrLn "\tOutput statevector:"
            putStrLn "\tsv <number of qubits> [qubits to negate]"
            putStrLn "\tExample: \"cabal run grover -- sv 4\""