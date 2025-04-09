module Main (
    main
  ) where

import Text.Lexer (alexScanTokens)
import Text.DslParser (dslParser)

main :: IO ()
main =
  putStrLn "Good to go"
