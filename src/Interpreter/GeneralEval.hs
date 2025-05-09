{-# LANGUAGE RankNTypes #-}

module Interpreter.GeneralEval where 
import Text.DslParser
import Interpreter.Csv 
import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)
import Data.List
import Data.Ord 
import Debug.Trace


type TableSlices = [(Ident, Row)]

type TableDataList = [TableData]

type TableData = ((Ident,FilePath),Table)


debugExpr x = traceShow x "HI"

-- evaluate the select statement by chaining together all the possible table operations
evalSelectStmt :: SelectStmt -> IO Table
evalSelectStmt (Select optDist cols tbl optJcs optConds optOrd optLimit optUnion) = do
  let (tableId, filePath) = evalTableName tbl
  initialTable <- readCSV filePath
  let tableDataList = [((tableId, filePath), initialTable)]

  joinsTDL <- case optJcs of
    Just jcs -> evalJoins jcs tableDataList
    Nothing  -> return tableDataList

  let condsTDL = case optConds of
        Just conds -> evalConditions joinsTDL conds
        Nothing    -> joinsTDL

  let orderTDL = case optOrd of
        Just orderc -> evalOrderBy orderc condsTDL
        Nothing     -> condsTDL

  let limitTDL = case optLimit of
        Just limit -> evalLimit limit orderTDL
        Nothing    -> orderTDL

  let colsTable = evalColumns cols limitTDL

  let distTable = case optDist of
        Just _  -> evalDistinct colsTable
        Nothing -> colsTable

  finalTable <- case optUnion of
        Just sstmt -> evalUnion distTable sstmt
        Nothing    -> return distTable

  return finalTable
  










-- Evaluates all the columns
evalColumns :: Columns -> TableDataList -> Table
evalColumns cols tds = case cols of
    (SelectAllColumns) -> transpose $ allColumns tds
    (SelectColumns cs) -> transpose $ selectedColumns cs tds

-- Helper function to evalColumns
allColumns :: TableDataList -> Table
allColumns tds = combineTables tbls
  where
  tbls = map (snd) tds

-- same funcition as the cartesion one in evaluator
combineTables :: [Table] -> Table
combineTables [] = []
combineTables (t:ts) = (transpose t) ++ combineTables ts

-- Helper function to evalColumns
selectedColumns :: [Column] -> TableDataList -> Table 
selectedColumns [] _ = []
selectedColumns (col:cols) tds = extractedCol:selectedColumns cols tds
  where
  extractedCol = case (extractColumn col) of 
      (value, -1) -> replicate (length (snd $ head tds)) value
      (identity, colIndex) ->  getColumn identity colIndex tds


evalDistinct :: Table -> Table
evalDistinct tbl = nub tbl

evalUnion :: Table -> SelectStmt -> IO Table
evalUnion tbl stmt = do
  evaledSelect <- evalSelectStmt stmt
  return (combineTables (tbl : evaledSelect : []))











-- Evaluate all the join clauses by feeding the output of one into the next
evalJoins :: [JoinClause] -> TableDataList -> IO TableDataList
evalJoins [] tds = return tds
evalJoins (j:js) tds = do 
  evaledJoin <- evalJoin j tds
  evalJoins js (evaledJoin)


-- Splits the components of the JoinClause into its most basic parts
extractJoin :: JoinClause -> (String, TableName, OnCondition)
extractJoin jc = (joinType, tblName, onCond)
  where
  (joinType, tblName, onCond) = case jc of
    InnerJoin t c -> ("Inner", t, c)
    LeftJoin t c -> ("Left", t, c)
    RightJoin t c -> ("Right", t, c)
    FullJoin t c -> ("Full", t, c)
    CrossJoin (TableAlias fp ident) -> ("Cross", (TableAlias fp ident), (ColEquals (ColumnByIndex ident 0) (ColumnByIndex "*" 0))) -- This needs work
  

-- Evaluates the join clause by taking in the current tables and changing them
evalJoin :: JoinClause -> TableDataList -> IO TableDataList
evalJoin jc tds = do 


  let (joinType, tableName, onCond) = extractJoin jc
  let (tableIdent,filePath) = evalTableName tableName -- get the tables info

  let alreadyExists = any (\((ident,fp),_) -> (ident == tableIdent) || (fp == filePath)) tds -- checks if table already in tds

  newTblsData <- (case alreadyExists of
    True -> return tds -- If already exists, dont add it to the list
    False -> do -- If it does not exist then, read the csv and add the table to the list
      loadedTbl <- readCSV filePath
      let tblData = ((tableIdent,filePath),loadedTbl)
      return (tds ++ (tblData:[])) )
  
  let (condFunc,(col1,col2)) = evalOnCondition onCond -- get the on condition component parts
  let (leftCol,rightCol) = identifyLRTbls tableIdent col1 col2
  putStrLn (show (leftCol,rightCol))
  let joinedTblsData = helperJoin newTblsData joinType tableIdent condFunc leftCol rightCol -- send to helperJoin todo the rest 

  return joinedTblsData


identifyLRTbls :: Ident -> Column -> Column -> (Column,Column)
identifyLRTbls ident col1 col2 = (left,right)
  where
  id1 = case (extractColumn col1) of
    (value, -1) -> error ("Cannot join on a value based column of " ++ value) 
    (identity, _) ->  identity

  id2 = case (extractColumn col2) of
    (value, -1) -> error ("Cannot join on a value based column of " ++ value) 
    (identity, _) ->  identity

  (left,right) = case (ident == id1) of
    True -> (col2,col1)
    False -> (col1, col2)


-- Continues evaluating the join
helperJoin :: TableDataList -> String -> Ident -> (String -> String -> Bool) -> Column -> Column -> TableDataList
helperJoin tds joinType joinTblIdent onCondFunc leftCol rightCol = (case joinType of
  -- For each different join type do the relavent function
  "Inner" -> makeTableDataList lId rId (innerJoin indexLCol indexRCol onCondFunc) tds resetData
  "Left" -> makeTableDataList lId rId (leftJoin indexLCol indexRCol onCondFunc) tds resetData
  "Right" -> makeTableDataList rId lId (rightJoin indexLCol indexRCol onCondFunc) tds resetData
  "Full" -> makeTableDataList lId rId (fullJoin indexLCol indexRCol onCondFunc) tds resetData
  "Cross" -> makeCrossTDL rId (crossJoin crossLeftCol indexRCol) tds resetData)

  where
    (lId,evaledLeftColumn) = case (extractColumn leftCol) of -- get the left/first column in the on condition 
      (value, -1) -> error ("Cannot join on a value based column of " ++ value) 
      (identity, colIndex) ->  (identity, getColumn identity colIndex tds) -- get the table containing the colum and the column

    (rId,evaledRightColumn) = case (extractColumn rightCol) of -- get the right/second column in the on condition
      (value, -1) -> error ("Cannot join on a value based column of " ++ value) 
      (identity, colIndex) ->  (identity, getColumn identity colIndex tds) -- get the table containing the colum and the column

    indexLCol = zip [0..] evaledLeftColumn -- index the values of the column
    indexRCol = zip [0..] evaledRightColumn -- index the values of the column

    crossLeftCol = zip [0..] (findLeftColumnForCross tds)
    --y = debugExpr (indexLCol,indexRCol)

    resetData = map (\(info,tbl) -> (info,[])) tds -- gives tds with each table being cleared

findLeftColumnForCross :: TableDataList -> [String]
findLeftColumnForCross tds = head $ transpose (snd $ head tds)


getTableById :: Ident -> TableDataList -> Table
getTableById ident [] = error ("Table Identifier: " ++ ident ++ " does not exist")
getTableById ident (((identity,_),tbl):tds) | ident == identity = tbl
                                     | otherwise = getTableById ident tds


-- Evaluates the OnCondition into a more suitable structure
evalOnCondition :: OnCondition -> ((String -> String -> Bool),(Column, Column))
evalOnCondition (ColEquals col1 col2) = ((==),(col1,col2))
evalOnCondition (ColNotEquals col1 col2) = ((/=),(col1,col2))
evalOnCondition (ColLessThan col1 col2) = ((<),(col1,col2))
evalOnCondition (ColGreaterThan col1 col2) = ((>),(col1,col2))
evalOnCondition (ColLessThanEq col1 col2) = ((<=),(col1,col2))
evalOnCondition (ColGreaterThanEq col1 col2) = ((>=),(col1,col2))



-- Custom check to see if two values satisfy a condition. The row index if true, and -1 if false
check :: String -> String -> Int -> (String -> String -> Bool) -> Int
check val1 val2 index f | f val1 val2 = index
                        | otherwise = -1

-- Computes which rows in first csv matchs to which rows in the second csv, and gives back their indexes in pairs
innerJoin :: [(Int, String)] -> [(Int, String)] -> (String -> String -> Bool) -> [(Int,Int)]
innerJoin [] _ _ = []
innerJoin ((leftI,leftVal):ls) rs f = [(leftI,rightI) | (rightI,rightVal) <- rs, (f leftVal rightVal)] ++ (innerJoin ls rs f)

-- Computes which rows in first csv matchs to which rows in the second csv, and gives back their indexes in pairs
leftJoin :: [(Int, String)] -> [(Int, String)] -> (String -> String -> Bool) -> [(Int,Int)]
leftJoin [] _ _ = []
leftJoin ((leftI,leftVal):ls) rs f = [(leftI, check leftVal rightVal rightI f) | (rightI, rightVal) <- rs] ++ (leftJoin ls rs f)

-- Computes which rows in first csv matchs to which rows in the second csv, and gives back their indexes in pairs
rightJoin ::[(Int, String)] -> [(Int, String)] -> (String -> String -> Bool) -> [(Int,Int)]
rightJoin _ [] _ = []
rightJoin ls ((rightI,rightVal):rs) f = [(rightI,check rightVal leftVal leftI f) | (leftI, leftVal) <- ls] ++ (rightJoin ls rs f)

-- Computes which rows in first csv matchs to which rows in the second csv, and gives back their indexes in pairs
fullJoin :: [(Int, String)] -> [(Int, String)] -> (String -> String -> Bool) -> [(Int,Int)]
fullJoin ls rs f = nub ((leftJoin ls rs f) ++ (rightJoin ls rs f))

-- Computes which rows in first csv matchs to which rows in the second csv, and gives back their indexes in pairs
crossJoin :: [(Int, String)] -> [(Int, String)] -> [(Int,Int)]
crossJoin ls rs = [(i1,i2) | (i1,_) <- ls, (i2,_) <- rs]


-- Iterates over the matching rows and makes a new table data list based on it
makeTableDataList :: Ident -> Ident -> [(Int,Int)] -> TableDataList -> TableDataList -> TableDataList
makeTableDataList _ _ [] _ accTbls = accTbls
makeTableDataList leftId rightId ((li,ri):as) tds accTbls = makeTableDataList leftId rightId as tds updated 
  
  
  where
  lrow = (getTableById leftId tds) !! li -- gets the row of index li from the current table data list
  rrow = case ri of -- gets the row of index ri from the current table data list 
    (-1) -> replicate (getTableArity rightId tds) "" -- if the index (ri) is invalid fill the space with empty strings
    (r) -> (getTableById rightId tds) !! r

  updated = updateTblsData rrow rightId (updateTblsData lrow leftId accTbls) -- update the new table data list
  

makeCrossTDL :: Ident -> [(Int,Int)] -> TableDataList -> TableDataList -> TableDataList
makeCrossTDL _ [] _ accTbls = accTbls
makeCrossTDL rightId ((li,ri):as) tds accTbls = makeCrossTDL rightId as tds updatedCross

  where
  rrow = case ri of -- gets the row of index ri from the current table data list 
    (-1) -> replicate (getTableArity rightId tds) "" -- if the index (ri) is invalid fill the space with empty strings
    (r) -> (getTableById rightId tds) !! r

  fTDs = filter (\((id,_),_) -> id /= rightId) accTbls
  allRows = getAllRows ri fTDs

  updatedCross = updateTblsData rrow rightId (updateCross allRows accTbls)
  

getTableArity :: Ident -> TableDataList -> Int
getTableArity ident tds = length $ head (getTableById ident tds)

getRow :: Ident -> Int -> TableDataList -> [String]
getRow ident rNum tds = (getTableById ident tds) !! rNum

getAllRows :: Int -> TableDataList -> [(Ident,[String])]
getAllRows row tds = firstFullRow
  where
  firstFullRow = map (\((id,_),tbl) -> (id,(tbl !! row))) tds

getColumn :: Ident -> Int -> TableDataList -> [String]
getColumn ident colNum tds = (transpose (getTableById ident tds)) !! colNum


getTableDataById :: Ident -> TableDataList -> TableData
getTableDataById ident [] = error ("Table Identifier: " ++ ident ++ " does not exist")
getTableDataById ident (((identity,fp),tbl):tds) | ident == identity = ((identity,fp),tbl)
                                     | otherwise = getTableDataById ident tds





updateTblsData :: [String] -> Ident -> TableDataList -> TableDataList
updateTblsData _ _ [] = []
updateTblsData row identity (((ident,fp),tbl):tds) | ident == identity = (((ident,fp),added):tds)
                                                   | otherwise = ((ident,fp),tbl):updateTblsData row identity tds
  where added = reverse (row:(reverse tbl))


updateCross :: [(Ident,[String])] -> TableDataList -> TableDataList
updateCross [] acc = acc
updateCross ((ident,row):rows) acc = updateCross rows (updateTblsData row ident acc)








-- Evaluates all the conditions how it works:
-- Filters rows from the Cartesian product that satisfy all conditions.
  -- Filters rows from the Cartesian product that satisfy all conditions.
evalConditions :: TableDataList -> [Condition] -> TableDataList
evalConditions [((alias, filePath), table)] conds =
  let
    filteredRows = filter (\row -> all (\cond -> rowSatisfies row cond [((alias, filePath), table)]) conds) table -- if only one table like task 2 for example 
  in
    [((alias, filePath), filteredRows)]

--Handling multiple tables
evalConditions tablesData conds =
  let
    allTables = map snd tablesData
    fullRows  = cartesianProduct allTables
    filteredRows = filter (\row -> all (\cond -> rowSatisfies row cond tablesData) conds) fullRows
    splitRows = map (`splitRowByTables` tablesData) filteredRows
    groupedByAlias = groupByAlias splitRows
  in
    rebuildTableDataList groupedByAlias tablesData



-- Checks whether a combined row satisfies a given condition, using correct table slices by alias.
rowSatisfies :: Row -> Condition -> TableDataList -> Bool
rowSatisfies fullRow cond tablesData =
  case cond of
    Equals col val        -> compareValue (getValue col) val (==)
    NotEquals col val     -> compareValue (getValue col) val (/=)
    LessThan col val      -> compareValue (getValue col) val (<)
    GreaterThan col val   -> compareValue (getValue col) val (>)
    LessThanEq col val    -> compareValue (getValue col) val (<=)
    GreaterThanEq col val -> compareValue (getValue col) val (>=)
    InList col vals       -> any (\v -> compareValue (getValue col) v (==)) vals
    Between col v1 v2     -> compareValue (getValue col) v1 (>=) &&
                             compareValue (getValue col) v2 (<=)
  where
    -- Associate each alias with its chunk of the flat row
    tableSlices :: [(Ident, Row)]
    tableSlices = splitRowByTables fullRow tablesData

    -- Lookup the correct value from the correct table slice
    getValue :: Column -> String
    getValue (ColumnByIndex alias idx) =
      case lookup alias tableSlices of
        Just subRow -> safeIndex idx subRow
        Nothing     -> error $ "Alias '" ++ alias ++ "' not found in row."
    getValue (ColumnByValue val _) = evalValue val


-- Get the value at the given index and returns empty string if index out of bounds
safeIndex :: Int -> [String] -> String
safeIndex i xs = if i < length xs then xs !! i else ""

-- compares a string with a Value using the given comparison operator, returning False if types don’t match.
compareValue :: String -> Value -> (forall a. Ord a => a -> a -> Bool) -> Bool
compareValue s v op = case compareVal s v op of
  Just b  -> b
  Nothing -> False


-- Compares a string with a value within in the Maybe context
compareVal :: String -> Value -> (forall a. Ord a => a -> a -> Bool) -> Maybe Bool
compareVal colStr value f = 
  case value of
    ValInteger num -> do
      parsed <- readMaybe colStr :: Maybe Int
      return $ f parsed num
    ValNumber num -> do
      parsed <- readMaybe colStr :: Maybe Double
      return $ f parsed num
    ValString str ->
      return $ f colStr str
    ValIdent ident ->
      return $ f colStr ident


extractColumn :: Column -> (Ident, Int)
extractColumn (ColumnByIndex ident index) = (ident,index)
extractColumn (ColumnByValue val ident) = (evalValue val, -1)


-- Evaluates the Value into a more suitable structure
evalValue :: Value -> String
evalValue (ValString str) = str
evalValue (ValInteger int) = show int
evalValue (ValNumber double) = show double
evalValue (ValIdent ident) = ident


-- Evaluates the TableNames into a more suitable structure
evalTableName :: TableName -> (Ident,FilePath)
evalTableName (TableAlias filePath ident) = (ident,filePath)



--Sorts a combined table based on the ORDER BY clause and reassigns rows back to their original table aliases.
evalOrderBy :: OrderClause -> TableDataList -> TableDataList
evalOrderBy orderClause tablesData =
  let
    extractKey = buildSortKeyExtractor orderClause tablesData
    combinedTable = cartesianProduct (map snd tablesData)
    sorted = case orderClause of
      OrderByAsc _  -> sortBy (compareRows extractKey) combinedTable
      OrderByDesc _ -> sortBy (flip $ compareRows extractKey) combinedTable
    slicedRows = map (`splitRowByTables` tablesData) sorted
    grouped = groupByAlias slicedRows
  in
    rebuildTableDataList grouped tablesData


-- Builds a function that extracts the sort key a String from a row based on the given OrderClause and table structure.
buildSortKeyExtractor :: OrderClause -> TableDataList -> (Row -> String)
buildSortKeyExtractor clause tables =
  case clause of
    OrderByAsc col  -> \row -> extractValueFromColumn col row tables
    OrderByDesc col -> \row -> extractValueFromColumn col row tables

-- Retrieves the value from the specified column in a row by looking up the corresponding table alias and index.
extractValueFromColumn :: Column -> Row -> TableDataList -> String
extractValueFromColumn (ColumnByIndex alias idx) row tables =
  case lookup alias (splitRowByTables row tables) of
    Just subRow -> safeIndex idx subRow
    Nothing     -> error $ "Alias '" ++ alias ++ "' not found."
extractValueFromColumn (ColumnByValue val _) _ _ = evalValue val

-- this compares the rows 
compareRows :: (Row -> String) -> Row -> Row -> Ordering
compareRows keyFn r1 r2 = compare (keyFn r1) (keyFn r2)

-- Applies the LIMIT clause to the combined rows from all tables, then reconstructs the result into a TableDataList.
evalLimit :: LimitClause -> TableDataList -> TableDataList
evalLimit limitClause tablesData =
  let
    combinedTable = cartesianProduct (map snd tablesData)
    limited = case limitClause of
      Limit n -> take n combinedTable
      LimitOffset offset n -> take n (drop offset combinedTable)
    sliced = map (`splitRowByTables` tablesData) limited
    grouped = groupByAlias sliced
  in
    rebuildTableDataList grouped tablesData

  
--Groups rows by their alias, combining all rows that belong to the same alias into one list.
groupByAlias :: [TableSlices] -> [(Ident, [Row])]
groupByAlias slices = foldr insertRow [] (concat slices)
  where
    insertRow (alias, row) [] = [(alias, [row])]
    insertRow (alias, row) ((a, rs):rest)
      | alias == a = (a, row:rs) : rest
      | otherwise  = (a, rs) : insertRow (alias, row) rest


--Reconstructs a TableDataList by attaching filtered rows to their original aliases and file paths from the old data.
rebuildTableDataList :: [(Ident, [Row])] -> TableDataList -> TableDataList
rebuildTableDataList grouped oldData =
  [ ((alias, findFilePath alias), reverse rows) | (alias, rows) <- grouped ]
  where
    findFilePath a = case lookup a (map (\((id, fp), _) -> (id, fp)) oldData) of
                       Just fp -> fp
                       Nothing -> error $ "Missing alias: " ++ a

-- Generates all row combinations from multiple tables
cartesianProduct :: [Table] -> Table
cartesianProduct [] = [[]]
cartesianProduct (t:ts) = [row1 ++ row2 | row1 <- t, row2 <- cartesianProduct ts]

-- Splits a flat cartesian row into slices corresponding to each table's alias.
splitRowByTables :: Row -> TableDataList -> [(Ident, Row)]
splitRowByTables row tables = go row tables []
  where
    go [] [] acc = reverse acc
    go r (((alias, _), t):ts) acc =
      let n = if null t then 0 else length (head t)
          (chunk, rest) = splitAt n r
      in go rest ts ((alias, chunk):acc)
    go _ _ _ = error "Row and tables mismatch"

  