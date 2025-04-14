module Interpreter.Evaluater where 
import Text.DslParser
import Interpreter.Csv 
import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)
type IndexPair = (Int, Int)


evaluate :: SelectStatement -> IO Table
-- Handles the Cartesian product task1 query  
evaluate (Select _ SelectAllColumns tableList Nothing _ _ _ _ _) = do
  tables <- mapM loadTable tableList
  let result = cartesianProduct tables
  return (sortTable result)
-- Handles a select query with a specific columns like queries in  t2 and t3  
evaluate
  (Select _ (SelectColumns selectedCols)
           [TableAlias fileName _]
           Nothing
           (Just conditions)
           Nothing Nothing Nothing Nothing) = do
  table <- readCSV fileName
  let filtered = filterRows table conditions
  let projected = projectColumns selectedCols filtered
  return (sortTable projected)
-- Handles a specific columns without using WHERE like query in t4
evaluate
  (Select _ (SelectColumns selectedCols)
           [TableAlias fileName _]
           Nothing
           Nothing
           Nothing Nothing Nothing Nothing) = do
  table <- readCSV fileName
  let projected = projectColumns selectedCols table
  return (sortTable projected)
-- Handles the LEFTMERGE query for task5 
evaluate 
  (Select _ (SelectColumns selectedCols) 
           [TableAlias fileP _, TableAlias fileQ _]
           _
           (Just conditions)
           _
           _
           _
           (Just LeftMerge)  
  ) = do
    pTable <- readCSV fileP
    qTable <- readCSV fileQ
    let merged = leftMerge pTable qTable conditions
    let projected = projectColumns selectedCols merged
    return (sortTable projected)


-- load a csv file from the given table.
loadTable :: TableName -> IO Table
loadTable (TableAlias fileName _) = readCSV fileName

-- Computes the Cartesian product of a list of tables 
cartesianProduct :: [Table] -> Table
cartesianProduct [] = [[]]
cartesianProduct (t:ts) = [row1 ++ row2 | row1 <- t, row2 <- cartesianProduct ts]


-- extract the  column from each row based on a list of ColumnIdent 
projectColumns :: [Column] -> Table -> Table
projectColumns cols table = map (projectRow cols) table

-- Extracts and returns the selected column values from a row.
projectRow :: [Column] -> Row -> Row
projectRow cols row = map (extractValue row) cols

-- Extracts the string value of a column from a row based on column type.
extractValue :: Row -> Column -> String
extractValue row (ColumnIdent name) = row !! columnIndex name
extractValue _   (ColumnByValue (ValString s) _) = s
extractValue _   (ColumnByValue (ValInteger i) _) = show i
extractValue _   (ColumnByValue (ValNumber n) _)  = show n
extractValue _   _ = error "Unsupported column projection"

-- A helper function used in both filtering and projection logic  
columnIndex :: String -> Int
columnIndex col
  | Just n <- readMaybe (drop 1 col), head col `elem` ['a', 'p', 'q'] = n - 1
  | otherwise = error ("Invalid column name: " ++ col)


-- Filters rows based on equality conditions 
filterRows :: Table -> [Condition] -> Table
filterRows table conds = filter (\row -> all (rowSatisfies row) conds) table


-- for each row it checks if the condition is true or not
rowSatisfies :: Row -> Condition -> Bool
rowSatisfies row (Equals (ColumnIdent c1) (ValIdent c2)) =
  (row !! columnIndex c1) == (row !! columnIndex c2)
rowSatisfies row (NotEquals (ColumnIdent c1) (ValIdent c2)) =
  (row !! columnIndex c1) /= (row !! columnIndex c2)
rowSatisfies row (NotEquals (ColumnIdent c1) (ValString s)) =
  (row !! columnIndex c1) /= s
rowSatisfies row (Equals (ColumnIdent c1) (ValString s)) =
  (row !! columnIndex c1) == s
rowSatisfies _ _ = False


-- Merges two rows by replacing empty values in the first row with values from the second.
leftMergeRow :: Row -> Row -> Row
leftMergeRow p q = zipWith (\pVal qVal -> if null pVal then qVal else pVal) p q


-- In this function what we do is we check all equality conditions between the two tables based on the query.
leftMerge :: Table -> Table -> [Condition] -> Table
leftMerge pTable qTable conds =
  case extractMatchingPairs conds of
    [] -> []  
    pairs -> 
      [ leftMergeRow pRow qRow
      | pRow <- pTable
      , qRow <- qTable
      , rowsMatchOnPairs pRow qRow pairs
      ]

-- Extracts index pairs from equality conditions for matching columns between two tables.
extractMatchingPairs :: [Condition] -> [IndexPair]
extractMatchingPairs = mapMaybe extractPair
  where
    extractPair (Equals (ColumnIdent c1) (ValIdent c2)) =
      Just (columnIndex c1, columnIndex c2)
    extractPair _ = Nothing

-- Checks if all column pairs from two rows have equal values based on index pairs.
rowsMatchOnPairs :: Row -> Row -> [IndexPair] -> Bool
rowsMatchOnPairs pRow qRow pairs =
  all (\(i, j) -> safeGet i pRow == safeGet j qRow) pairs

-- Get the value at the given index and returns empty string if index out of bounds
safeGet :: Int -> Row -> String
safeGet i row = if i < length row then row !! i else ""



