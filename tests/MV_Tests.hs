module MV_Tests (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)

import LinAlg
import Gates
import EvalMV

import Data.Vector as V
import Data.List as L
import Data.Set as S
import Control.Monad.ST

comparePureTensors (PTIV {scalarIV = scalar1, qbsIV = qbs1}) (PTIV {scalarIV = scalar2, qbsIV = qbs2}) =
  scalar1 == scalar2 && V.all (uncurry (~=)) (V.zip qbs1 qbs2)

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
    "EvalMV" [
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

          -- testCase "Always apply control-gate if control bits are 1" (
          --   let pt = PT 1 $ V.fromList [qubit 0 1, qubit 0 1 qubit 0.3 0.7]
          --       expected = PT 1 $ V.fromList [qubit 0 1, qubit 0 1 qubit 0.7 0.3] 
          --       res = eval (Ctrl (S.fromList [0]) 1 X) 1 pt
          --   in assertBool "Tensors not identical" $ compareTensors res [pt]
          -- ),

          -- testCase "if ∥⟨1|qc⟩∥^2 = 1 =⇒ ⟨0|qc⟩ = 0" (
          --   let pt = PT 1 $ V.fromList [qubit 0 1, qubit 0.3 0.7]
          --       expectedPT = PT 1 $ V.fromList [qubit 0 1, qubit 0.7 0.3]
          --       res = evalTerm (C [0] 1 X) pt
          --   in assertBool "Tensors not identical" $ compareTensors res [expectedPT]
          -- ),

          -- testCase "if G(qt) = qt" (
          --   let pt = PT 1 $ V.fromList [qubit 0.5 0.5, qubit 0.5 0.5]
          --       res = evalTerm (C [0] 1 X) pt
          --   in assertBool "Tensors not identical" $ compareTensors res [pt]
          -- ),

          -- testCase "𝛼 · (𝑞1 ⊗ . . . ⊗ 𝑞𝑛) + 𝛽 · (𝑞1 ⊗ . . . ⊗ 𝑞𝑛) = (𝛼 + 𝛽) · (𝑞1 ⊗ . . . ⊗ 𝑞𝑛)" (
          --   let pt = [PT 1 $ V.fromList [qubit 0.5 0.5, qubit 0.5 0.5], PT 1 $ V.fromList [qubit 0.5 0.5, qubit 0.5 0.5]]
          --       exptected = [PT 2 $ V.fromList [qubit 0.5 0.5, qubit 0.5 0.5]] 
          --       res = evalProgram [C [0] 1 X] pt
          --   in assertBool "Tensors not identical" $ compareTensors res exptected
          -- )
        ]
    ]
