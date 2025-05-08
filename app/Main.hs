module Main (main) where
  

import Text.Lexer (alexScanTokens)
import Text.DslParser (dslParser)
import Data.Maybe (isNothing, isJust)
import System.Environment (getArgs)
--import Interpreter.Evaluater
import Interpreter.Csv
import Interpreter.GeneralEval
import Text.DslParser (Stmt(StmtDelete))
import Text.DslParser (Stmt(StmtSelect))
import Text.DslParser (Stmt(StmtInsert))
import Text.DslParser (Stmt(StmtUpdate))



main :: IO ()
main = do
  args <- getArgs
  code <- case args of
    [filename] -> readFile filename
    _ -> error "Must take only one file"
  let tokens = alexScanTokens code
  let ast = dslParser tokens
  --putStrLn (show tokens)
  --putStrLn (show ast)
  case ast of
    StmtSelect sel -> do
      result <- evalSelectStmt sel
      --writeToCSV "output.csv" result
      printTable result
    StmtDelete del -> do
      result <- evalDeleteStmt del
      --writeToCSV "output.csv" result
      printTable result
    StmtInsert ins -> do 
     result <- evalInsertStmt ins
     --writeToCSV "output.csv" result
     printTable result 
    StmtUpdate upd -> do 
     result <- evalUpdateStmt upd
     --writeToCSV "output.csv" result
     printTable result

toIO :: a -> IO a
toIO x = return x
