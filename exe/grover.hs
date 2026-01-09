import EvalMV
import StateVector
import LinAlg
import Gates
import Data.Set as S


numberOfInputQubits :: Integer
numberOfInputQubits = 4

-- Need output qubit
qubits :: [Qubit]
qubits = [qubit (sqrt 1) (sqrt 0) | _ <- [0 .. (numberOfInputQubits-1)]]



inputQubits :: [Int]
inputQubits = [0 .. fromInteger (numberOfInputQubits-2)]

allQ = [0 .. fromInteger (numberOfInputQubits-1)] 

-- amplification
singleSolGroverCircuit :: [Gate]
singleSolGroverCircuit = [
    Sing H (S.fromList allQ),

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

-- numberOfRuns = floor $ (pi / 4) * sqrt numberOfInputQubits 

main :: IO ()
main = putStr $ ppSV $ tensorToStateVector $ eval singleSolGroverCircuit 1 qubits