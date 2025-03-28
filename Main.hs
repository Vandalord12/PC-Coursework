import Lexer (alexScanTokens)
import System.Environment (getArgs)
import Control.Exception (catch, SomeException)
import System.IO (readFile)
import DslParser (parseSQL)

main :: IO ()
main = catch main' noParse

main' :: IO ()
main' = do
    (fileName : _) <- getArgs 
    sourceText <- readFile fileName
    putStrLn ("Lexing : " ++ sourceText)
    let lexedProg = alexScanTokens sourceText
    putStrLn ("Lexed Tokens: " ++ show lexedProg)
    let parsedProg = parseSQL (lexedProg)
    putStrLn ("Parsed as " ++ (show parsedProg) ++ "\n")

noParse :: SomeException -> IO ()
noParse e = putStrLn ("Parse error: " ++ show e)
