{-# LANGUAGE RankNTypes #-}

module Interpreter.GeneralEval where 
import Text.DslParser
import Interpreter.Csv 
import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)
import Data.List
import Data.Ord 
import Data.List (sortBy)
import Data.Ord (comparing)
import Debug.Trace
import Debug.Trace (trace)

type TableSlices = [(Ident, Row)]

type TableDataList = [TableData]

type TableData = ((Ident,FilePath),Table)


debugExpr label x = trace (label ++ (show x)) x

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

  
  let (fullTable,outputTable) = evalColumns cols condsTDL
  
  putStrLn ("Cols: " ++ show outputTable)

  let orderTDL = case optOrd of
        Just orderc -> evalOrderBy orderc fullTable outputTable
        Nothing     -> outputTable
  
  let limitTDL = case optLimit of
        Just limit -> evalLimit limit orderTDL
        Nothing    -> orderTDL

  let converted = convertToTable limitTDL
  
  let distTable = case optDist of
        Just _  -> evalDistinct converted
        Nothing -> converted

  finalTable <- case optUnion of
        Just sstmt -> evalUnion distTable sstmt
        Nothing    -> return distTable

  return finalTable
  


--evaluate the DELETE statement by Deleting the rows matching conditions and returns the remaining table.
evalDeleteStmt :: DeleteStmt -> IO Table
evalDeleteStmt (Delete tableName maybeConds) = do
  let (ident, filePath) = evalTableName tableName
  table <- readCSV filePath
  let filtered = case maybeConds of
        Nothing -> [] 
        Just conds ->
          let tdl = [((ident, filePath), table)]
              keepRow row = not $ all (\cond -> rowSatisfies row cond tdl) conds
          in filter keepRow table
  return filtered



-- Evaluate the INSERT statement by appending one or more rows to the CSV table.
-- Note it won't allow you insert more columns than what the exsisting file contain.
evalInsertStmt :: InsertStmt -> IO Table
evalInsertStmt (Insert filePath rowsToInsert) = do
  rows <- readCSV filePath

  let columnCount = if null rows then length (head rowsToInsert) else length (head rows)

  let convertedRows = map
        (\vals ->
          if length vals /= columnCount
            then error $ "Row has " ++ show (length vals) ++ " values, expected " ++ show columnCount
            else map evalValue vals
        )
        rowsToInsert

  let updated = rows ++ convertedRows

  return updated


-- Evaluates an UPDATE statement by applying changes to rows that match the conditions and returns the modified table.
evalUpdateStmt :: UpdateStmt -> IO Table
evalUpdateStmt (Update filePath alias assignments conds) = do
  table <- readCSV filePath
  let assignIndexVals = [(idx, val) | ((_, idx), val) <- assignments]
  let tableData = [((alias, filePath), table)] 

  let updatedRows = map
        (\row ->
          if all (\cond -> rowSatisfies row cond tableData) conds
            then applyUpdates row assignIndexVals
            else row
        )
        table

  return updatedRows

  where
    applyUpdates row updates =
      [ case lookup i updates of
          Just val -> evalValue val
          Nothing  -> oldVal
      | (i, oldVal) <- zip [0..] row
      ]



clean :: TableDataList -> TableDataList
clean tds = filter (\((_,_),tbl) -> not $ null tbl) tds



-- Evaluates all the columns
evalColumns :: Columns -> TableDataList -> (TableDataList, TableDataList)
evalColumns (SelectAllColumns) tds = (tds,tds)
evalColumns (SelectColumns cs) tds = (fullTD, correctedTD)
  where 
  (fullTD, outputTD) = (computeColumns cs tds (map (\(info,tbl) -> (info,[])) tds))
  correctedTD = map ((\(info,tbl) -> (info,transpose tbl))) outputTD
 


computeColumns :: [Column] -> TableDataList -> TableDataList -> (TableDataList, TableDataList)
computeColumns [] tds accTDs = (tds,accTDs)
computeColumns (col:cols) tds accTDs = computeColumns cols tds newTblsData 
  where
  (ident,_) = extractColumn col
  evaledCol = evalColumn col tds
  
  alreadyExists =  any (\((id,_),_) -> (ident == id)) tds -- checks if table already in resetData

  newTblsData = case alreadyExists of
    False -> accTDs ++ (((ident,""),[evaledCol]):[])
    True -> updateTblsData evaledCol ident accTDs


-- transposeTblData :: Ident -> TableDataList -> TableDataList
-- transposeTblData _ [] = []
-- transposeTblData identity (((ident,fp),tbl):tds) | ident == identity = (((ident,fp),transpose tbl):tds)
--                                                    | otherwise = ((ident,fp),tbl):transposeTblData identity tds

evalCoalesce :: Column -> Column -> TableDataList -> [String]
evalCoalesce c1 c2 tds =
  let col1 = evalColumn c1 tds
      col2 = evalColumn c2 tds
  in zipWith (\x y -> if x == "" then y else x) col1 col2


evalColumn :: Column -> TableDataList -> [String]
evalColumn col tds = evaledColumn
  where
  evaledColumn = case col of
    ColumnByValue val ident -> replicate (length (snd $ head tds)) (evalValue val)
    ColumnByIndex ident index -> debugExpr "byIndex: " (getColumn ident index tds)
    ColumnCoalesce col1 col2 _ -> evalCoalesce col1 col2 tds








    




evalDistinct :: Table -> Table
evalDistinct tbl = nub tbl

evalUnion :: Table -> SelectStmt -> IO Table
evalUnion tbl stmt = do
  evaledSelect <- evalSelectStmt stmt
  return (combineTables (tbl : evaledSelect : []))





convertToTable :: TableDataList -> Table
convertToTable tds = transpose $ combineTables tbls
  where
  tbls = map (snd) tds


combineTables :: [Table] -> Table
combineTables [] = []
combineTables (t:ts) = (transpose t) ++ combineTables ts












-- Evaluate all the join clauses by feeding the output of one into the next
evalJoins :: [JoinClause] -> TableDataList -> IO TableDataList
evalJoins [] tds = return tds
evalJoins (j:js) tds = do 
  evaledJoin <- evalJoin j tds
  putStrLn ("Join:  " ++ show evaledJoin)
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
    CrossJoin t c -> ("Cross", t, c) -- This needs work
  

-- Evaluates the join clause by taking in the current tables and changing them
evalJoin :: JoinClause -> TableDataList -> IO TableDataList
evalJoin jc tds = do
  let (joinType, tableName, onCond) = extractJoin jc
  let (tableIdent, filePath) = evalTableName tableName -- get the table's info

  let alreadyExists = any (\((ident, fp), _) -> ident == tableIdent || fp == filePath) tds
  newTblsData <- case alreadyExists of
    True -> return tds -- If already exists, don't add it to the list
    False -> do
      loadedTbl <- readCSV filePath
      let tblData = ((tableIdent, filePath), loadedTbl)
      return (tds ++ [tblData])

  let (condFunc, (col1, col2)) = evalOnCondition onCond
  let (leftCol, rightCol) = identifyLRTbls tableIdent col1 col2

  putStrLn ("Columns loaded: " ++ show (leftCol, rightCol))
  putStrLn("Tables List: " ++ show newTblsData)
  return $ helperJoin newTblsData joinType tableIdent condFunc leftCol rightCol


identifyLRTbls :: Ident -> Column -> Column -> (Column,Column)
identifyLRTbls ident col1 col2 = (left,right)
  where
  id1 = case (extractColumn col1) of
    (ident, -1) -> error ("Cannot join on a value based column of identity: " ++ ident) 
    (identity, _) ->  identity

  id2 = case (extractColumn col2) of
    (ident, -1) -> error ("Cannot join on a value based column of identity: " ++ ident) 
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
  "Cross" -> crossJoin lId rId tds)

  where
    (lId,evaledLeftColumn) = case (extractColumn leftCol) of -- get the left/first column in the on condition 
      (ident, -1) -> error ("Cannot join on a value based column of identity: " ++ ident) 
      (identity, colIndex) ->  (identity, getColumn identity colIndex tds) -- get the table containing the colum and the column

    (rId,evaledRightColumn) = case (extractColumn rightCol) of -- get the right/second column in the on condition
      (ident, -1) -> error ("Cannot join on a value based column of identity: " ++ ident) 
      (identity, colIndex) ->  (identity, getColumn identity colIndex tds) -- get the table containing the colum and the column

    indexLCol = zip [0..] evaledLeftColumn -- index the values of the column
    indexRCol = zip [0..] evaledRightColumn -- index the values of the column

  
    resetData = map (\(info,tbl) -> (info,[])) tds -- gives tds with each table being cleared


crossJoin :: Ident -> Ident -> TableDataList -> TableDataList
crossJoin lId rId tds = updateTblData lId ctbl tds
  where
  ltbl = case getTableById lId tds of
    Just tbl -> tbl
    Nothing -> []
  rtbl = case getTableById rId tds of
    Just tbl -> tbl
    Nothing -> []
  ctbl = crossTables ltbl rtbl

crossTables :: Table -> Table -> Table
crossTables t1 t2 = [row1 ++ row2 | row1 <- t1, row2 <- t2]

updateTblData :: Ident -> Table -> TableDataList -> TableDataList
updateTblData _ _ [] = []
updateTblData identity newTbl (((ident,fp),tbl):tds) | ident == identity = (((ident,fp),newTbl):tds)
                                                      | otherwise = ((ident,fp),tbl):updateTblData identity newTbl tds



getTableById :: Ident -> TableDataList -> Maybe Table
getTableById ident [] = Nothing
getTableById ident (((identity,_),tbl):tds) | ident == identity = (Just tbl)
                                            | otherwise = getTableById ident tds


-- Evaluates the OnCondition into a more suitable structure
evalOnCondition :: OnCondition -> ((String -> String -> Bool),(Column, Column))
evalOnCondition (OnColEquals col1 col2) = ((==),(col1,col2))
evalOnCondition (OnColNotEquals col1 col2) = ((/=),(col1,col2))
evalOnCondition (OnColLessThan col1 col2) = ((<),(col1,col2))
evalOnCondition (OnColGreaterThan col1 col2) = ((>),(col1,col2))
evalOnCondition (OnColLessThanEq col1 col2) = ((<=),(col1,col2))
evalOnCondition (OnColGreaterThanEq col1 col2) = ((>=),(col1,col2))



-- Custom check to see if two values satisfy a condition. The row index if true, and -1 if false
check :: String -> String -> Int -> (String -> String -> Bool) -> Int
check val1 val2 index f | f val1 val2 = index
                        | otherwise = -1

-- Computes which rows in first csv matchs to which rows in the second csv, and gives back their indexes in pairs
innerJoin :: [(Int, String)] -> [(Int, String)] -> (String -> String -> Bool) -> [(Int,Int)]
innerJoin [] _ _ = []
innerJoin ((leftI,leftVal):ls) rs f = [(leftI,rightI) | (rightI,rightVal) <- rs, (debugExpr ("Values: (" ++ leftVal ++ ", " ++ rightVal ++ ") Outcome: ") (f leftVal rightVal))] ++ (innerJoin ls rs f)

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



-- Iterates over the matching rows and makes a new table data list based on it
makeTableDataList :: Ident -> Ident -> [(Int,Int)] -> TableDataList -> TableDataList -> TableDataList
makeTableDataList _ _ [] _ accTbls = clean accTbls
makeTableDataList leftId rightId ((li,ri):as) tds accTbls = makeTableDataList leftId rightId as tds updated 
  
  
  where
  lrow = case (getTableById leftId tds) of
    Just tbl -> tbl !! li -- gets the row of index li from the current table data list
    Nothing -> []
  rrow = case ri of -- gets the row of index ri from the current table data list 
    (-1) -> replicate (getTableArity rightId tds) "" -- if the index (ri) is invalid fill the space with empty strings
    (r) -> case (getTableById rightId tds) of 
      Just tbl -> tbl !! r
      Nothing -> []

  updated = updateTblsData rrow rightId (updateTblsData lrow leftId accTbls) -- update the new table data list




getTableArity :: Ident -> TableDataList -> Int
getTableArity ident tds = (case getTableById ident tds of
  Just tbl -> length $ head tbl
  Nothing -> 0)


getAllRows :: Int -> TableDataList -> [(Ident,[String])]
getAllRows row tds = firstFullRow
  where
  firstFullRow = map (\((id,_),tbl) -> (id,accessTbl tbl row)) tds
  accessTbl :: Table -> Int -> Row
  accessTbl tbl row | (row >= length tbl)  = []
                    | otherwise = tbl !! row

--this gets all the rows across the tables. If the table is not large enough then pad with spaces


getColumn :: Ident -> Int -> TableDataList -> [String]
getColumn ident colNum tds | colNum >= (length (transpose t))  = []
                           | otherwise = (transpose t) !! colNum
  where 
  t = (case getTableById ident tds of 
        Just tbl -> tbl
        Nothing -> [])


getTableDataById :: Ident -> TableDataList -> TableData
getTableDataById ident [] = error ("Table Identifier: " ++ ident ++ " does not exist")
getTableDataById ident (((identity,fp),tbl):tds) | ident == identity = ((identity,fp),tbl)
                                     | otherwise = getTableDataById ident tds





updateTblsData :: [String] -> Ident -> TableDataList -> TableDataList
updateTblsData _ _ [] = []
updateTblsData row identity (((ident,fp),tbl):tds) | ident == identity = (((ident,fp),added):tds)
                                                   | otherwise = ((ident,fp),tbl):updateTblsData row identity tds
  where added = reverse (row:(reverse tbl))


updateAllRows :: [(Ident,[String])] -> TableDataList -> TableDataList
updateAllRows [] acc = acc
updateAllRows ((ident,row):rows) acc = updateAllRows rows (debugExpr "updatedTable: " (updateTblsData row ident acc))








-- Evaluates all the conditions how it works:
-- Filters rows from the Cartesian product that satisfy all conditions.
  -- Filters rows from the Cartesian product that satisfy all conditions.

evalConditions :: TableDataList -> [Condition] -> TableDataList
evalConditions tablesData conds =
  let
    -- Sort the tables by alias so A comes before B
    sortedTables = sortBy (comparing (fst . fst)) tablesData

    -- Combine rows by row index, not cartesian product
    joinedRows = map concat (transpose (map snd sortedTables))

    -- Filter based on WHERE
    filteredRows = filter (\row -> all (\cond -> rowSatisfies row cond sortedTables) conds) joinedRows

    -- Re-split by alias
    splitRows = map (`splitRowByTables` sortedTables) filteredRows
    grouped = groupByAlias splitRows
  in
    rebuildTableDataList grouped sortedTables







-- Checks whether a combined row satisfies a given condition, using correct table slices by alias.
rowSatisfies :: Row -> Condition -> TableDataList -> Bool
rowSatisfies fullRow cond tablesData =
  case cond of
    -- Column = Value comparisons
    Equals col val        -> compareValue (getValue col) val (==)
    NotEquals col val     -> compareValue (getValue col) val (/=)
    LessThan col val      -> compareValue (getValue col) val (<)
    GreaterThan col val   -> compareValue (getValue col) val (>)
    LessThanEq col val    -> compareValue (getValue col) val (<=)
    GreaterThanEq col val -> compareValue (getValue col) val (>=)

    ColEquals col1 col2        -> getValue col1 == getValue col2
    ColNotEquals col1 col2     -> getValue col1 /= getValue col2
    ColLessThan col1 col2      -> getValue col1 <  getValue col2
    ColGreaterThan col1 col2   -> getValue col1 >  getValue col2
    ColLessThanEq col1 col2    -> getValue col1 <= getValue col2
    ColGreaterThanEq col1 col2 -> getValue col1 >= getValue col2

    -- Other conditions
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
extractColumn (ColumnByValue val ident) = (ident, -1)
extractColumn (ColumnCoalesce c1 c2 ident) = (ident, -2)



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
evalOrderBy :: OrderClause -> TableDataList -> TableDataList -> TableDataList -- needs to get tested
evalOrderBy (OrderAll) tds outTDs | (length $ clean tds) < 1 = []
                                  | otherwise = orderedTDL

  where
  allRows = convertToTable outTDs
  indexedRows = zip [0..] allRows
  ordered = debugExpr "orderedRows: " (sortBy (\(_, b1) (_, b2) -> compare b1 b2) indexedRows)


  resetData = map (\(info,tbl) -> (info,[])) outTDs -- gives tds with each table being cleared
  orderedTDL = remakeDataList (map (fst) ordered) outTDs resetData

evalOrderBy orderBy tds outTDs = orderedTDL
  where 
  (column,dir) = case orderBy of 
    (OrderByAsc col) -> (col,"asc")
    (OrderByDesc col) -> (col, "desc")
  
  evaledColumn = case (extractColumn column) of 
    (ident, -1) -> error ("Cannot order on a value based column of identity " ++ ident) 
    (identity, colIndex) ->  getColumn identity colIndex tds

  indexedCol = zip [0..] evaledColumn
  
  ordered = (case dir of
    "asc" -> sortBy (\(_, b1) (_, b2) -> compare b1 b2) indexedCol
    "desc" -> sortBy (\(_, b1) (_, b2) -> compare b2 b1) indexedCol)
  
  resetData = map (\(info,tbl) -> (info,[])) tds -- gives tds with each table being cleared
  orderedTDL = remakeDataList (map (fst) ordered) outTDs resetData

  

remakeDataList :: [Int] -> TableDataList -> TableDataList -> TableDataList
remakeDataList [] _ accTDs = accTDs
remakeDataList (index:indexs) tds accTDs = remakeDataList indexs tds updated
  where
  allRows = debugExpr ("Rows at index " ++(show index) ++ ": ") (getAllRows index tds)
  updated = updateAllRows allRows accTDs






-- Applies the LIMIT clause to the combined rows from all tables, then reconstructs the result into a TableDataList.
evalLimit :: LimitClause -> TableDataList -> TableDataList -- not tested yet 
evalLimit limitClause tablesData =
  let
    joinedRows = map concat (transpose (map snd tablesData))
    limitedRows = case limitClause of
      Limit n -> take n joinedRows
      LimitOffset offset n -> take n (drop offset joinedRows)
    splitRows = map (`splitRowByTables` tablesData) limitedRows
    grouped = groupByAlias splitRows
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

  