import EvalMV
import StateVector
import LinAlg
import Gates
import Data.Set as S
import Data.List as L
import Debug.Trace

numberOfInputQubits :: Integer
numberOfInputQubits = 5

-- Need output qubit
qubits :: [Qubit]
qubits = [qubit (sqrt 1) (sqrt 0) | _ <- [0 .. (numberOfInputQubits-1)]]

inputQubits :: [Int]
inputQubits = [0 .. fromInteger (numberOfInputQubits-2)]

allQ = [0 .. fromInteger (numberOfInputQubits-1)]

-- amplification
singleSolGroverCircuit :: [Gate]
singleSolGroverCircuit = [

    -- Sing X (S.fromList [0]),

    -- Oracle
    Ctrl Z (S.fromList $ tail allQ) 0,

    -- Sing X (S.fromList [0]),

    -- -- Amplification circuit
    Sing H (S.fromList allQ),
    Sing X (S.fromList allQ),
    Ctrl Z (S.fromList $ tail allQ) 0,
    Sing X (S.fromList allQ),
    Sing H (S.fromList allQ)
  ]

numberOfRuns :: Int
numberOfRuns = floor $ ((pi :: Float) / (4 :: Float)) * sqrt (2 ^ numberOfInputQubits)

main :: IO ()
main = putStr $ ppSV $ tensorToStateVector $ eval (
    Sing H (S.fromList allQ) : concat (replicate (trace ("Hello" ++ show numberOfRuns) numberOfRuns) singleSolGroverCircuit)
  ) 1 qubits