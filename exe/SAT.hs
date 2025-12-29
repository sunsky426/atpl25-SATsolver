module Main where

import Parser
import Quantumize
import Grovers2
import Generator
import Eval
import Macros 
import Gates

main :: IO ()
main = 
  let 
      -- obtain input - substitute appropriate input stream later
      example = "p & q | (123 & ~123) & ( (x ^ y) ^ z)"

      -- parse input
      (bexp,n) = 
        case parseWithUnique example of
          Right x  -> x
          Left err -> error err

      -- quantumize boolean expression
      (instrs,m) = compile bexp
      qop = quantumize (n,m) instrs

      -- apply Grover's algorithm
      width = n + m + 1
      groversCircuit = groverIteration qop (diffusion width) 1

      -- ???
      --h = evalProgram groversCircuit (zero width)

      -- profit
   in putStrLn $ show $ length groversCircuit
