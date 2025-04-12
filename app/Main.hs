module Main (
    main
  ) where

import Text.Lexer (alexScanTokens)
import Text.DslParser (dslParser)
import Data.Maybe (isNothing, isJust)

--main :: IO ()
main = do
  code <- readFile "resources/code.txt"
  tokens <- toIO (alexScanTokens code)
  ast <- toIO (dslParser tokens)
  putStrLn (show ast)
  return ast

toIO :: a -> IO a
toIO x = return x

--interpreter :: SelectStatement -> SelectStatement

interpreter stmt | isJust joins = withJoinEval j distinct files
where 
stmt = (Select distinct cols files joins conds order limit union)
justJoin = Just j

withJoinEval _ Nothing _ = 
withJoinEval (CrossJoin s) distinct files =
withJoinEval (LeftJoin col)
withJoinEval (RightJoin col)
withJoinEval (FullJoin col)
withJoinEval (InnerJoinC col cond)
withJoinEval (LeftJoinC col cond)
withJoinEval (RightJoinC col cond)
withJoinEval (FullJoinC col cond)



    