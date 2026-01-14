{-# OPTIONS_GHC -Wno-type-defaults #-}
module GenEval_Tests (tests) where

import GenEval

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool)

import Data.Vector as V
import Data.List as L
import Data.Set as S
import Control.Monad.ST
import Numeric.LinearAlgebra as NL

comparePureTensors :: PureTensorIV -> PureTensorIV -> Bool
comparePureTensors (PTIV {scalarIV = scalar1, qbsIV = qbs1}) (PTIV {scalarIV = scalar2, qbsIV = qbs2}) =
  scalar1 == scalar2 && V.all (uncurry (~=)) (V.zip qbs1 qbs2)

compareTensors :: [PureTensorIV] -> [PureTensorIV] -> Bool
compareTensors t1 t2 =
  L.all (uncurry comparePureTensors) $ L.zip t1 t2

evalPureTensors :: [Gate] -> [PureTensorIV] -> [PureTensorIV]
evalPureTensors circuit pts = runST evaluator
  where evaluator = do
          q <- Prelude.mapM toMV pts
          res <- evalCircuit circuit q
          Prelude.mapM toIV res

tests :: TestTree
tests =
  testGroup
    "Unit tests for general evaluator" [
      testGroup
        "Evaluation of gates"
        [
          testCase "Applying single qubit gates" (
            let qbl = [qubit 1 0, qubit (sqrt 0.5) (-(sqrt 0.5)), qubit (sqrt 0.5) (sqrt 0.5)]
                res = eval [
                  Sing H (S.fromList [0, 1, 2]),
                  Sing Z (S.fromList [0, 1, 2]),
                  Sing X (S.fromList [0, 1, 2])
                  ] 1 qbl
                expected = [PTIV 1 (V.fromList [qubit (-(sqrt 0.5)) (sqrt 0.5), qubit (-1) 0, qubit 0 1])]
            in assertBool "Tensors not identical" $ compareTensors res expected
          ),
          testCase "Applying control gates" (
            let qbl = [qubit (sqrt 0.5) (sqrt 0.5), qubit (sqrt 0.3) (sqrt 0.7), qubit (sqrt 0.3) (sqrt 0.7)]
                res = eval [
                    Ctrl X (S.fromList [0,1]) 2
                  ] 1 qbl
                diff = evalSingle X (qbl !! 2) - (qbl !! 2)
                gamma = sqrt $ dot (unQubit diff) (unQubit diff)
                expected = [
                    PTIV 1 (V.fromList qbl),
                    PTIV (sqrt 0.5 * sqrt 0.7 * gamma) (V.fromList [qubit 0 1, qubit 0 1, qubit (sqrt 0.5) (-(sqrt 0.5))])
                  ]
            in assertBool "Tensors not identical" $ compareTensors res expected
          )
        ],

      testGroup
        "Scaled pure tensors simplification"
        [
          testCase "Add scalars of pure tensors that are the same" (
            let pts = [
                    PTIV 3 (V.fromList [qubit (sqrt 0.5) (sqrt 0.5), qubit (sqrt 0.3) (sqrt 0.7)]),
                    PTIV 2 (V.fromList [qubit (sqrt 0.5) (sqrt 0.5), qubit (sqrt 0.3) (sqrt 0.7)])
                  ]
                expected = [PTIV 5 (V.fromList [qubit (sqrt 0.5) (sqrt 0.5), qubit (sqrt 0.3) (sqrt 0.7)])]
                res = evalPureTensors [] pts
            in assertBool "Tensors not identical" $ compareTensors res expected
          ),
          testCase "Add scalars of pure tensors that are multiples of each other (1/2)" (
            let pts = [
                    PTIV 3 (V.fromList [qubit (sqrt 0.8) (sqrt 0.2), qubit 0 1]),
                    PTIV 2 (V.fromList [qubit (sqrt 0.8) (sqrt 0.2), qubit 0 (-1)])
                  ]
                expected = [PTIV 1 (V.fromList [qubit (sqrt 0.8) (sqrt 0.2), qubit 0 1])]
                res = evalPureTensors [] pts
            in assertBool "Tensors not identical" $ compareTensors res expected
          ),
          testCase "Add scalars of pure tensors that are multiples of each other (2/2)" (
            let pts = [
                    PTIV 1 (V.fromList [qubit (sqrt 0.8) (sqrt 0.2), qubit 0 (-1)]),
                    PTIV 2 (V.fromList [qubit (sqrt 0.8) (sqrt 0.2), qubit 0 1]),
                    PTIV 3 (V.fromList [qubit (sqrt 0.8) (sqrt 0.2), qubit 0 (-1)]),
                    PTIV 4 (V.fromList [qubit (sqrt 0.8) (sqrt 0.2), qubit 0 1])
                  ]
                expected = [PTIV (-2) (V.fromList [qubit (sqrt 0.8) (sqrt 0.2), qubit 0 (-1)])]
                res = evalPureTensors [] pts
            in assertBool "Tensors not identical" $ compareTensors res expected
          )
        ],
      testGroup
        "Control gate simplification rules"
        [
          testCase "Do not apply control-gate if control bits are 0" (
            let qbl = [qubit 1 0, qubit 1 0, qubit 0.3 0.7]
                expected = [PTIV 1 (V.fromList qbl)]
                res = eval [Ctrl X (S.fromList [0, 1]) 2] 1 qbl
            in assertBool "Tensors not identical" $ compareTensors res expected
          ),

          testCase "Always apply control-gate if control bits are 1" (
            let qbl = [qubit 0 1, qubit 0 1, qubit 0.3 0.7]
                expected = [PTIV 1 $ V.fromList [qubit 0 1, qubit 0 1, qubit 0.7 0.3]]
                res = eval [Ctrl X (S.fromList [0, 1]) 2] 1 qbl
            in assertBool "Tensors not identical" $ compareTensors res expected
          ),

          testCase "Do not increase rank if gate has no effect" (
            let qbl = [qubit (sqrt 0.5) (sqrt 0.5), qubit (sqrt 0.5) (sqrt 0.5), qubit (sqrt 0.5) (sqrt 0.5)]
                expected = [PTIV 1 $ V.fromList qbl]
                res = eval [Ctrl X (S.fromList [0, 1]) 2] 1 qbl
            in assertBool "Tensors not identical" $ compareTensors res expected
          )
        ],
      testGroup
        "Misc"
        [
          testCase "Order does not matter when gates are independent" (
            let qbl = [qubit (sqrt 0.5) (sqrt 0.5) | _ <- [0 :: Integer .. 5]]
                circuit = [
                    Ctrl Z (S.fromList [0 .. 4]) 5,
                    Ctrl Z (S.fromList [0 .. 3]) 5,
                    Ctrl Z (S.fromList [0 .. 2]) 5,
                    Ctrl Z (S.fromList [0 .. 1]) 5,
                    Ctrl Z (S.fromList [0]) 5
                  ]
                res1 = eval circuit 1 qbl
                res2 = eval (L.reverse circuit) 1 qbl
                round' x = round $ realPart x * 1000000
            in
              assertBool "Number of pure tensors and result is the same" $ L.length res1 == L.length res2 && L.map round' (NL.toList $ tensorToStateVector res1) == L.map round' (NL.toList $ tensorToStateVector res2)
          )
        ]


    ]
