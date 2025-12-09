module Main where 

import AST
import Parser

main :: IO()
main = 
  let example = "p & q | (123 & ~456) & ( (x ^ y) ^ z)"
   in case parse example of
        Right x  -> putStrLn $ show x
        Left err -> putStrLn err
