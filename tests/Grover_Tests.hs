{- HLINT ignore "Use lambda-case" -}
module Grover_Tests (tests) where

import GenEval as GE
import SpecEval as SE
import Data.Vector as V
import Data.List as L
import Numeric.LinearAlgebra as NL
import Data.List as L
import qualified Data.Set as S
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, (@?=))

getBestSE :: [SE.PureTensor] -> String
getBestSE (_ : SE.PT _ qbs : _) =
  let l = V.toList qbs in
    L.map ((\qb -> case qb of
                    [amp0, amp1] | abs amp0 > abs amp1 -> '0'
                    [_, _] -> '1'
                    _ -> error "Expected qubit to have two elements") . NL.toList . SE.unQubit) l
getBestSE _ = error "Get best needs two pure tensors"


negList2sol :: [Int] -> Int -> [Char]
negList2sol nl qbC = negList2sol' nl 0
  where
    negList2sol' [] i = L.replicate (qbC - i) '1'
    negList2sol' (x:xs) i = if x == i then "0" L.++ negList2sol' xs (i+1) else "1" L.++ negList2sol' (x:xs) (i+1)

genNegLists :: (Ord a, Num a, Enum a) => a -> [[a]]
genNegLists n =
  L.map S.toList $ S.toList $ S.powerSet (S.fromList [0 .. n-1])

runSE :: Int -> [QubitPos] -> String
runSE qbCount nl =
    let oracle = [Only qp SE.X | qp <- nl] L.++ [MCZ [0..qbCount-1]] L.++ [Only qp SE.X | qp <- nl]
        groversCircuit = grovers qbCount oracle (numberOfRuns qbCount)
    in getBestSE $ SE.evalProgram groversCircuit (zero qbCount)

runGE :: Int -> [Int] -> String
runGE qbCount nl =
    let groversResult = runGroversSingleSol qbCount nl
    in  getBest groversResult

tests :: TestTree
tests =
  testGroup
    "Grovers simulation of one control-Z single solution circuits" [
      testGroup
        "General evaluator"
        (
          [testCase ("2 qubit oracle with negation list " L.++ show nl) $ runGE 2 nl @?= negList2sol nl 2 | nl <- genNegLists 2] L.++
          [testCase ("3 qubit oracle with negation list " L.++ show nl) $ runGE 3 nl @?= negList2sol nl 3 | nl <- genNegLists 3] L.++
          [testCase ("4 qubit oracle with negation list " L.++ show nl) $ runGE 4 nl @?= negList2sol nl 4 | nl <- genNegLists 4]
        ),
      testGroup
        "Specialized evaluator"
        (
          [testCase ("2 qubit oracle with negation list " L.++ show nl) $ runSE 2 nl @?= negList2sol nl 2 | nl <- genNegLists 2] L.++
          [testCase ("3 qubit oracle with negation list " L.++ show nl) $ runSE 3 nl @?= negList2sol nl 3 | nl <- genNegLists 3] L.++
          [testCase ("4 qubit oracle with negation list " L.++ show nl) $ runSE 4 nl @?= negList2sol nl 4 | nl <- genNegLists 4]
        )
    ]