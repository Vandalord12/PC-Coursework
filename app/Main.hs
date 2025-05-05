module Main (main) where
  

import Text.Lexer (alexScanTokens)
import Text.DslParser (dslParser)
import Data.Maybe (isNothing, isJust)
--import Interpreter.Evaluater
import Interpreter.Csv
import Interpreter.GeneralEval
import Text.DslParser (Stmt(StmtDelete))
import Text.DslParser (Stmt(StmtSelect))


main :: IO ()
main = do
  code <- readFile "resources/t2.cql" -- fix the task you want to test in here 
  let tokens = alexScanTokens code
  let ast = dslParser tokens
  putStrLn (show tokens)
  putStrLn (show ast)
  case ast of
    StmtSelect sel -> do
      result <- evalSelectStmt sel
      writeToCSV "resources/output.csv" result
    StmtDelete del -> do
      result <- evalDeleteStmt del
      writeToCSV "resources/output.csv" result  

toIO :: a -> IO a
toIO x = return x
