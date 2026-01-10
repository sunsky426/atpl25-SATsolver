module Main where

import ANF
import Comp
import Gates
import Measure
import Validation
import StateVector
import Parser

main :: IO ()
main = 
  let 
      -- -- obtain input - substitute appropriate input stream later
      example = "(a & b) ^ (b & c)"

      -- -- parse input
      (bexp,) = 
        case parseWithUnique example of
          Right x  -> x
          Left err -> error err

      -- -- quantumize boolean expression
      -- (instrs,m) = compile bexp
      -- qop = quantumize (n,m) instrs

      -- apply Grover's algorithm
      -- width = n + m + 1
      iterations = 5
      width = 3

      oracle = [Only 0 X, Only 2 X, Ctrl [0, 1] 2 Z, Only 2 X, Only 0 X]
      groversCircuit = grovers width oracle iterations

      -- ???
      -- h = evalProgram (pow H width) (zero width)
      result = evalByParts 1 groversCircuit (zero width)

      -- profit
   in print . vectorize =<< result
