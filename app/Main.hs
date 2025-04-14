module Main (main) where
  

import Text.Lexer (alexScanTokens)
import Text.DslParser (dslParser)
import Data.Maybe (isNothing, isJust)
import Interpreter.Evaluater
import Interpreter.Csv

--main :: IO ()
main = do
  code <- readFile "t4.cql" -- fix the task you want to test in here 
  tokens <- toIO (alexScanTokens code)
  ast <- toIO (dslParser tokens)
  print tokens
  putStrLn (show ast)
  result <- evaluate ast
  writeCSV result -- write to stdout in the csv form 

toIO :: a -> IO a
toIO x = return x
