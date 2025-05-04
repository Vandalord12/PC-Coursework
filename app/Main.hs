module Main (main) where
  

import Text.Lexer (alexScanTokens)
import Text.DslParser (dslParser)
import Data.Maybe (isNothing, isJust)
--import Interpreter.Evaluater
import Interpreter.Csv
import Interpreter.GeneralEval

main :: IO ()
main = do
  code <- readFile "resources/t2.cql" -- fix the task you want to test in here 
  let tokens = alexScanTokens code
  let ast = dslParser tokens
  putStrLn (show tokens)
  putStrLn (show ast)
  result <- evalSelectStmt ast
  writeToCSV "resources/output.csv" result -- write to stdout in the csv form 

toIO :: a -> IO a
toIO x = return x
