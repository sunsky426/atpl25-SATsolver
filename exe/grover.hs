import EvalMV
import StateVector
import LinAlg
import Gates
import Data.Set as S
import Data.List as L
import Debug.Trace
import System.Environment
import Criterion.Main

numberOfRuns :: Int -> Int
numberOfRuns qbCount = floor $ ((pi :: Float) / (4 :: Float)) * sqrt (2 ^ qbCount)

singleSol qbCount [] = [
    Ctrl Z (S.fromList $ tail [0 .. qbCount-1]) 0
  ]
singleSol qbCount negList = [
    Sing X (S.fromList negList),

    Ctrl Z (S.fromList $ tail [0 .. qbCount-1]) 0,

    Sing X (S.fromList negList)
  ]

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

-- main :: IO ()
-- main = 

  -- putStr $ getBest $ eval (

  -- ) 1 qubits

runGrovers qbCount negList =
  let oracle = singleSol qbCount negList
      groversCircuit = generateGroversCircuit oracle (fromIntegral qbCount)
  in  eval groversCircuit 1 [qubit (sqrt 1) (sqrt 0) | _ <- [0 .. (qbCount-1)]]

parseNegList negListStr qbCount
  | L.null negListStr = []
  | negListStr == ["all"] = [0 .. qbCount-1]
  | otherwise = read ("[" ++ head negListStr ++ "]") :: [Int]

main :: IO ()
main = do
    args <- getArgs
    case args of
        "bench":qbCountStr:_ -> do
          let qbCount = read qbCountStr
              benchList = [
                  bench (show i ++ " qubits") $ nf (getBest . runGrovers i) $ parseNegList ["all"] i
                  | i <- [2 .. qbCount]
                ]
          -- print benchList
          withArgs (L.drop 2 args) $ defaultMain [ 
              bgroup "single solution grover" benchList
            ]
        "sv":qbCountStr:negListStr -> do
          let qbCount = read qbCountStr
              groversResult = runGrovers qbCount (parseNegList negListStr qbCount)
          putStr $ ppSV $ tensorToStateVector groversResult
        qbCountStr:negListStr -> do
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