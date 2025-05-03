module Interpreter.Csv where

import Data.Char (isSpace)
import System.IO (readFile)
import Data.List (intercalate)
import Data.List (sort)


--  A Row is set of strings.
type Row = [String]

-- A Table is consist of list of rows.
type Table = [Row]

-- Trim leading and trailing whitespace from a string
trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace


  -- This function usese trim and splitComma as a helper functions to put splited words in a list without leading or trailing spaces (a csv row).
parseRow :: String -> Row
parseRow line = map (trim . unquote) (splitComma line)


--  A helper function to be called at the parseRow function to split words by a comma
splitComma :: String -> [String]
splitComma [] = [""]
splitComma (',':xs) = "" : splitComma xs
splitComma (x:xs) =
  let (y:ys) = splitComma xs
  in (x:y):ys

-- read a csv file and return a Table consist list of rows.
readCSV :: FilePath -> IO Table
readCSV path = do
  contents <- readFile path
  let allLines = lines contents                 -- Split file into lines
      parsed = map parseRow allLines       -- Parse each line into a Row
  return parsed


-- Converts a single Row list of Strings into a csv format 
formatRow :: Row -> String
formatRow = intercalate ","  -- joins entries with commas

-- Write the Table to stdout in CSV format
writeCSV :: Table -> IO ()
writeCSV table = mapM_ (putStrLn . formatRow) table




-- Converts a list of strings 
toCSVRow :: [String] -> String
toCSVRow row = intercalate "," row  -- Join the elements with commas

-- Converts a list of rows 
toCSV :: [[String]] -> String
toCSV rows = unlines $ map toCSVRow rows  -- Join rows with newlines

-- A function to write the CSV content to a file
writeToCSV :: FilePath -> [[String]] -> IO ()
writeToCSV path rows = writeFile path (toCSV rows)



-- Sort a table lexicographically
sortTable :: Table -> Table
sortTable = sort

-- Remove the double quotes  
unquote :: String -> String
unquote s
  | length s >= 2 && head s == '"' && last s == '"' = init (tail s)
  | otherwise = s


