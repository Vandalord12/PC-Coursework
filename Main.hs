import Lexer
import System.Environment
import Control.Exception
import System.IO

main :: IO ()
main = catch main' noParse

main' = do (fileName : _ ) <- getArgs 
           sourceText <- readFile fileName
           putStrLn ("Lexing : " ++ sourceText)
           let lexedProg = alexScanTokens sourceText
           putStrLn ("Lexed Tokens:" ++ lexedProg)
           -- let parsedProg = parse (lexedLog)
           -- putStrLn ("Parsed as " ++ (show parsedProg) ++ "\n")
           -- let result = evalLoop (parsedProg)
           -- putStrLn ("Evaluates to " ++ (unparse result) ++ "\n")
