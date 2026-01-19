{- HLINT ignore "Use camelCase" -}
import HQP

import HQP.QOp.StatevectorSemantics
import System.Environment
import Criterion.Main

startState qbCount = ket [0 .. qbCount]

numberOfRuns :: Int -> Int
numberOfRuns qbCount = floor $ ((pi :: Float) / (4 :: Float)) * sqrt (2 ^ qbCount)

simple_oracle qbCount = [Unitary $ foldl (\x y -> y x) Z [C | _ <- [0 .. qbCount-2]]]

overlap_oracle qbCount = [
    Unitary $ Tensor (foldl (\x y -> y x) Z [C | _ <- [0 .. tqb]]) (Id (qbCount-2 - tqb))
    | tqb <- [0 .. qbCount-2] 
  ]

complex_oracle qbCount = [
    Unitary $ Tensor (Id tqb) (Tensor (C Z) (Id (qbCount-2 - tqb)))
    | tqb <- [0 .. qbCount-2] 
  ]

groverCircuit oracle qbCount =
  [ Unitary $ foldl Tensor H [H | _ <- [0 .. qbCount-2]]] ++
  oracle ++ 
  concat (replicate (numberOfRuns qbCount) [
    Unitary $ foldl Tensor H [H | _ <- [0 .. qbCount-2]],
    Unitary $ foldl Tensor X [X | _ <- [0 .. qbCount-2]],
    Unitary $ foldl (\x y -> y x) Z [C | _ <- [0 .. qbCount-2]],
    Unitary $ foldl Tensor X [X | _ <- [0 .. qbCount-2]],
    Unitary $ foldl Tensor H [H | _ <- [0 .. qbCount-2]]
  ])

runGrovers oracle qbCount =
  evalProg (groverCircuit oracle qbCount) (ket [0 | _ <- [0..qbCount-1]]) []

convertAnswer (_, bl, _) = map (\b -> if b then '1' else '0') bl

main :: IO ()
main = do
    args <- getArgs
    case args of
        "bench-complex":qbCountStr:_ -> do
          let maxQbCount = read qbCountStr
              benchList = [
                  bench (show qbCount ++ " qubits") $ nf ((\(_, l, _) -> l) . runGrovers (complex_oracle qbCount)) qbCount
                  | qbCount <- [2 .. maxQbCount]
                ]
          withArgs (drop 2 args) $ defaultMain [
              bgroup "Grovers with complex oracle using state vector semantics" benchList
            ]
        "bench-overlap":qbCountStr:_ -> do
          let maxQbCount = read qbCountStr
              benchList = [
                  bench (show qbCount ++ " qubits") $ nf ((\(_, l, _) -> l) . runGrovers (overlap_oracle qbCount)) qbCount
                  | qbCount <- [2 .. maxQbCount]
                ]
          withArgs (drop 2 args) $ defaultMain [
              bgroup "Grovers with gate overlapping oracle using state vector semantics" benchList
            ]
        "bench-simple":qbCountStr:_ -> do
          let maxQbCount = read qbCountStr
              benchList = [
                  bench (show qbCount ++ " qubits") $ nf ((\(_, l, _) -> l) . runGrovers (simple_oracle qbCount)) qbCount
                  | qbCount <- [2 .. maxQbCount]
                ]
          withArgs (drop 2 args) $ defaultMain [
              bgroup "Grovers with overlapping control-Z gates oracle using state vector semantics" benchList
            ]
        qbCountStr:_ ->
          let qbCount = read qbCountStr in
          putStrLn $ convertAnswer $ runGrovers (simple_oracle qbCount) qbCount
        _ -> error "Argument not handles"