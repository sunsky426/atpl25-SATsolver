module Main where

import AST
import ANF
import Comp
import Grovers
import Validation
import Gates
import Parser
import EvalMV
import LinAlg
import Grovers
import StateVector
import System.Environment (getArgs)
import Data.List (intersperse)
import qualified Data.Set as S
import qualified Data.Vector as V

-- deprecate once gate types are no longer separate
changeGateType :: QGate -> Gate
changeGateType qp =
  case qp of
    Single op pos -> Sing op $ S.singleton pos
    C pos1 pos2 op-> Ctrl op (S.fromList pos1) pos2
    CZ (pos:[]) -> Sing Z $ S.singleton pos
    CZ (pos:pos') -> Ctrl Z (S.fromList pos') pos
    CZ [] -> error "fun"


--unique :: Exp -> Int
--unique e =
--  case e of
--    Const _ -> 0
--    Var i -> i+1
--    AND e1 e2 -> max (unique e1) (unique e2)
--    OR e1 e2 -> max (unique e1) (unique e2)
--    XOR e1 e2 -> max (unique e1) (unique e2)
--    NEG e' -> unique e'


solve :: Int -> Exp -> Solution
solve n bexp =
  let oracle = phaseOracle bexp
      groverSingleIteration = pow H n ++ groverIteration oracle (diffusion n) 1
      groverByDef = grover oracle n
      grovers = map changeGateType groverSingleIteration--groverSingleIteration
      zeroTens = replicate n (qubit 1 0)
      resultingTensor = eval grovers 1 zeroTens
   in outcome $ tensorToStateVector resultingTensor
      --do 
      --  putStrLn $ ppSV $ tensorToStateVector resultingTensor
      --  putStrLn $ show $ outcome $ tensorToStateVector resultingTensor
      --  putStrLn $ show $ oracle
      --  putStrLn $ show $ translateAstToAnf bexp
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
          --solution = solve n bexp
       in do solve n bexp --ppResult solution vars
    _ -> do putStrLn "Usage: cabal run \"<boolexp>\""
