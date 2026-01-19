module Main where

import GenEval
import System.Environment (getArgs)
import Data.List (intersperse)

solve :: Int -> Exp -> Solution
solve n bexp =
  let (truth,oracle) = phaseOracle bexp
      --grovers = pow H n ++ groverIteration oracle (diffusion n) 1
      grovers = grover oracle n
      --zeroTens = replicate n (qubit 1 0)
      ts = evalProgram grovers n
   in if truth then mostLikelyBits n ts else leastLikelyBits n ts
      --resultingTensor = eval grovers 1 zeroTens
      --outcome $ tensorToStateVector resultingTensor
      --do 
      --  putStrLn $ show $ resultingTensor
      --  putStrLn $ ppSV $ tensorToStateVector resultingTensor
      --  putStrLn $ show $ outcome $ tensorToStateVector resultingTensor
      --  putStrLn $ show $ oracle
      --  putStrLn $ show $ bexp
      --  putStrLn $ show $ astToAnf bexp

ppResult :: Solution -> [String] -> IO ()
ppResult sol vars = do
  putStr $ concat $ intersperse ", " $ map (\(s,v) -> v ++ " -> " ++ show s) $ zip sol vars

main :: IO ()
main = do
  args <- getArgs
  case args of
    [input] -> 
      let (bexp,vars) = parseWithUnique input
          n = length vars
          solution = solve n bexp
       in ppResult solution vars
    _ -> do putStrLn "Usage: cabal run \"<boolexp>\""
