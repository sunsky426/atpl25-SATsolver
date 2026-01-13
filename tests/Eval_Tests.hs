module Eval_Tests (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool)

import LinAlg
import Gates
import Eval

import Data.Vector as V
import Data.List as L

comparePureTensors :: PureTensor -> PureTensor -> Bool
comparePureTensors (PT {scalar = scalar1, qbs = qbs1}) (PT {scalar = scalar2, qbs = qbs2}) =
  scalar1 == scalar2 && V.all (uncurry (~=)) (V.zip qbs1 qbs2)

compareTensors :: [PureTensor] -> [PureTensor] -> Bool
compareTensors t1 t2 =
  L.all (uncurry comparePureTensors) $ L.zip t1 t2

tests :: TestTree
tests =
  testGroup
    "evalTerm"
    [
      testCase "if ∥⟨0|qc⟩∥^2 = 1 =⇒ ⟨1|qc⟩ = 0" (
        let pt = PT 1 $ V.fromList [qubit 1 0, qubit 0.3 0.7]
            res = evalTerm (C [0] 1 X) pt
        in assertBool "Tensors not identical" $ compareTensors res [pt]
      ),

      testCase "if ∥⟨1|qc⟩∥^2 = 1 =⇒ ⟨0|qc⟩ = 0" (
        let pt = PT 1 $ V.fromList [qubit 0 1, qubit 0.3 0.7]
            expectedPT = PT 1 $ V.fromList [qubit 0 1, qubit 0.7 0.3]
            res = evalTerm (C [0] 1 X) pt
        in assertBool "Tensors not identical" $ compareTensors res [expectedPT]
      ),

      testCase "if G(qt) = qt" (
        let pt = PT 1 $ V.fromList [qubit 0.5 0.5, qubit 0.5 0.5]
            res = evalTerm (C [0] 1 X) pt
        in assertBool "Tensors not identical" $ compareTensors res [pt]
      ),

      testCase "𝛼 · (𝑞1 ⊗ . . . ⊗ 𝑞𝑛) + 𝛽 · (𝑞1 ⊗ . . . ⊗ 𝑞𝑛) = (𝛼 + 𝛽) · (𝑞1 ⊗ . . . ⊗ 𝑞𝑛)" (
        let pt = [PT 1 $ V.fromList [qubit 0.5 0.5, qubit 0.5 0.5], PT 1 $ V.fromList [qubit 0.5 0.5, qubit 0.5 0.5]]
            exptected = [PT 2 $ V.fromList [qubit 0.5 0.5, qubit 0.5 0.5]] 
            res = evalProgram [C [0] 1 X] pt
        in assertBool "Tensors not identical" $ compareTensors res exptected
      )
    ]
