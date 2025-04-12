module Csv where

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
      nonEmptyLines = filter (not . null) allLines  -- Skip blank lines
      parsed = map parseRow nonEmptyLines       -- Parse each line into a Row
  return parsed


-- Converts a single Row list of Strings into a csv format 
formatRow :: Row -> String
formatRow = intercalate ","  -- joins entries with commas

-- Write the Table to stdout in CSV format
writeCSV :: Table -> IO ()
writeCSV table = mapM_ (putStrLn . formatRow) table


-- Sort a table lexicographically
sortTable :: Table -> Table
sortTable = sort

-- Remove the double quotes  
unquote :: String -> String
unquote s
  | length s >= 2 && head s == '"' && last s == '"' = init (tail s)
  | otherwise = s


getColumnByIndex :: FilePath -> Int -> IO [String]
getColumnByIndex fileName index = do
  contents <- readCSV fileName
  let output = getColByIndex contents index
  return output
  where
    getColByIndex :: Table -> Int -> [String]
    getColByIndex [] _ = []
    getColByIndex (row:rows) index = (row !! index):getColByIndex rows index

getRowByIndex :: String -> Int -> IO [String]
getRowByIndex
getSpecificValue :: String -> Int -> Int -> [String]

columnFinder :: Column -> [String]
columnFiner (ColumnByIndex tbl num) = do 