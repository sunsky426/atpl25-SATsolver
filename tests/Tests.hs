module Tests (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)

import AST
import ANF
import Gates
import Comp

tests :: TestTree
tests =
  testGroup "General Pipeline Tests"
    [
      testGroup
        "AST -> ANF" [
          ---
          testCase "a | b" $
            astToAnf (OR (Var 0) (Var 1))
              @?= (Xor (Xor (Pos 0) (Pos 1)) (And (Pos 0) (Pos 1))),
          ---
          testCase "a & b" $
            astToAnf (AND (Var 0) (Var 1))
              @?= (And (Pos 0) (Pos 1)),
          ---
          testCase "a ^ b" $
            astToAnf (XOR (Var 0) (Var 1))
              @?= (Xor (Pos 0) (Pos 1)),
          ---
          testCase "~(a & b)" $
            astToAnf (NEG (AND (Var 0) (Var 1)))
              @?= (Xor (And (Pos 0) (Pos 1)) (Cst True))
      ],
      ---
      testGroup
        "AST -> PhaseOracle" [
          ---
          testCase "a | b" $
            phaseOracle (OR (Var 0) (Var 1))
              @?= [Single Z 0,Single Z 1, CZ [0,1]],
          ---
          testCase "a & b" $
            phaseOracle (AND (Var 0) (Var 1))
              @?= [CZ [0,1]],
          ---
          testCase "a ^ b" $
            phaseOracle (XOR (Var 0) (Var 1))
              @?= [Single Z 0, Single Z 1],
          ---
          testCase "~(a & b)" $
            phaseOracle (NEG (AND (Var 0) (Var 1)))
              @?= [CZ [0,1], Single Z 0]
      ],
      testGroup
        "Complete Pipeline Tests"
        [
        ]
    ]
