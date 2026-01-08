module Main where

-- import Parser
import Grovers
import Eval
import Gates
import Measure

main :: IO ()
main = 
  let 
      width = 4
      iterations = 1

      oracle = [MCZ [0, 1], MCZ [1, 2], MCZ [1, 2, 3]]
      groversCircuit = grovers width oracle iterations

      -- ???
      -- h = evalProgram (pow H width) (zero width)
      result = evalByParts 5 groversCircuit (zero width)
      -- profit
   in print . toBin . greedyMeasure . vectorize =<< result
