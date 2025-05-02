{-# LANGUAGE RankNTypes #-}

module Interpreter.GeneralEval where 
import Text.DslParser
import Interpreter.Csv 
import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)
import Data.List
import Data.Ord 

type TableAlias = (FilePath, Ident)
-- I assume that only Identifier[Int] is the only way to access a column
-- data SelectStmt = Select (Maybe Distinct) Columns TableName (Maybe JoinClause) (Maybe [Condition]) (Maybe OrderClause) (Maybe LimitClause) (Maybe SelectStmt) (Maybe MergeMode)
type TableDataList = [TableData]

type TableData = ((Ident,FilePath),Table)



-- -- Evaluates all the columns
-- evalColumns :: Columns -> TableName -> [JoinClause] -> IO [[String]]
-- evalColumns cols tbl jcs = case cols of
--     (SelectAllColumns) -> allColumns tbl
--     (SelectColumns cs) -> selectedColumns cs tbl jcs


-- -- Helper function to evalColumns
-- allColumns :: TableName -> IO [[String]]
-- allColumns tbl = do

--   let (filePath,_) = evalTableName tbl

--   allCols <- readCSV filePath
--   return allCols

-- -- Helper function to evalColumns
-- selectedColumns :: [Column] -> TableName -> [JoinClause] -> IO [[String]]
-- selectedColumns [] _ _ = return []
-- selectedColumns (col:cols) tbl jcs = do
--   evaledColumn <- topLvlEvalColumn col tbl jcs
--   rest <- selectedColumns cols tbl jcs
--   return (evaledColumn : rest)
  




-- Evaluate all the join clauses by feeding the output of one into the next
evalJoins :: TableDataList -> [JoinClause] -> IO TableDataList
evaledJoin _ [] = []
evalJoins tds (j:js) = evalJoins (evalJoin tds j) js


-- Splits the components of the JoinClause into its most basic parts
extractJoin :: JoinClause -> (String, TableName, OnCondition)
extractJoin jc = (joinType, tblName, onCond)
  where
  (joinType, tblName, onCond) = case jc of
    InnerJoin t c -> ("Inner", t, c)
    LeftJoin t c -> ("Left", t, c)
    RightJoin t c -> ("Right", t, c)
    FullJoin t c -> ("Full", t, c)
    CrossJoin t -> ("Cross", t, (ColEquals (ColumnByIndex "null" 0) (ColumnByIndex "null" 0))) -- This needs work
  

-- Evaluates the join clause by taking in the current tables and changing them
evalJoin :: TableDataList -> JoinClause -> IO TableDataList
evalJoin tds jc = do 


  let (joinType, tableName, onCond) = extractJoin jc
  let (filePath,tableIdent) = evalTableName tableName -- get the tables info

  let alreadyExists = any (\((id,fp),_) -> (id == tableIdent) || (fp == filePath)) tds -- checks if table already in tds

  newTblsData <- case alreadyExists of
    True -> return tds -- If already exists, dont add it to the list
    False -> do -- If it does not exist then, read the csv and add the table to the list
      loadedTbl <- readCSV filePath
      let tblData = ((tableIdent,filePath),loadedTbl)
      return tblData:tds 
  
  (condFunc,(leftCol,rightCol)) = evalOnCondition onCond -- get the on condition component parts
  let joinedTblsData = helperJoin newTblsData joinType tableIdent condFunc leftCol rightCol -- send to helperJoin todo the rest 

  return joinedTblsData




-- Continues evaluating the join
helperJoin :: TableDataList -> String -> Ident -> (String -> String -> Bool) -> Column -> Column -> TableDataList
helperJoin tds joinType joinTblIdent onCondFunc leftCol rightCol = (case joinType of
  -- For each different join type do the relavent function
  "Inner" -> makeTableDataList lId rId (innerJoin indexLCol indexRCol onCondFunc) tds resetData
  "Left" -> makeTableDataList lId rId (leftJoin indexLCol indexRCol onCondFunc) tds resetData
  "Right" -> makeTableDataList lId rId (rightJoin indexLCol indexRCol onCondFunc) tds resetData
  "Full" -> makeTableDataList lId rId (fullJoin indexLCol indexRCol onCondFunc) tds resetData
  "Cross" -> makeTableDataList lId rId (crossJoin indexLCol indexRCol) tds resetData)

  where
    (lId,evaledLeftColumn) = case (extractColumn leftCol) of -- get the left/first column in the on condition 
      (value, -1) -> error ("Cannot join on a value based column of " ++ value) 
      (ident, colIndex) ->  (ident, getColumn ident colIndex tds) -- get the table containing the colum and the column

    (rId,evaledRightColumn) = case (extractColumn rightCol) of -- get the right/second column in the on condition
      (value, -1) -> error ("Cannot join on a value based column of " ++ value) 
      (ident, colIndex) ->  (ident, getColumn ident colIndex tds) -- get the table containing the colum and the column

    indexLCol = zip [0..] evaledLeftColumn -- index the values of the column
    indexRCol = zip [0..] evaledRightColumn -- index the values of the column

    resetData = map (\(info,tbl) -> (info,[])) tds -- gives tds with each table being cleared
    



getTableById :: Ident -> TableDataList -> Table
getTableById id [] = error ("Table Identifier: " ++ id ++ " does not exist")
getTableById id (((ident,_),tbl):tds) | id == ident = tbl
                                     | otherwise = getTableById id tds


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
innerJoin ((leftI,leftVal):ls) rs f = [(leftI,rightI) | (rightI,rightVal) <- rs, (f leftVal rightVal)]

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
fullJoin ls rs f = nub $ (leftJoin ls rs f) ++ (rightJoin ls rs f)


-- Computes which rows in first csv matchs to which rows in the second csv, and gives back their indexes in pairs
crossJoin :: [(Int, String)] -> [(Int, String)] -> [(Int,Int)]
crossJoin ls rs = [(i1,i2) | (i1,_) <- ls, (i2,_) <- rs]




-- Iterates over the matching rows and makes a new table data list based on it
makeTableDataList :: Ident -> Ident -> [(Int,Int)] -> TableDataList -> TableDataList -> TableDataList
makeTableDataList _ _ [] accTbls = accTbls
makeTableDataList leftId rightId ((li,ri):as) tds accTbls = makeTableDataList leftId rightId as tds updated 
  
  
  where
  lrow = (getTableById leftId tds) !! li -- gets the row of index li from the current table data list
  rrow = case ri of -- gets the row of index ri from the current table data list 
    (-1) -> replicate (getTableArity rightId tds) "" -- if the index (ri) is invalid fill the space with empty strings
    (r) -> (getTableById rightId tds) !! r

  updated = updateTblsData rrow rightId (updateTblsData lrow leftId accTbls) -- update the new table data list
  


getTableArity :: Ident -> TableDataList -> Int
getTableArity id tds = length $ head (getTableById id)

getRow :: Ident -> Int -> TableDataList -> [String]
getRow id rNum tds = (getTableById id tds) !! rNum

getColumn :: Ident -> Int -> TableDataList -> [String]
getColumn id colNum tds = (transpose (getTableById id tds)) !! colNum

updateTblsData :: [String] -> Ident -> TableDataList -> TableDataList
updateTblsData _ _ [] = []
updateTblsData row ident (((id,fp),tbl):tds) | id == ident = (((id,fp),added):tds)
                                             | otherwise = (((id,fp),tbl):tds):updateTblsData row ident tds
  where added = reverse (row:(reverse tbl))








extractColumn :: Column -> (Ident, Int)
extractColumn (ColumnByIndex ident index) = (ident,index)
extractColumn (ColmnByValue val ident) = (evalValue val, -1)


-- Evaluates the Value into a more suitable structure
evalValue :: Value -> String
evalValue (ValString str) = str
evalValue (ValInteger int) = show int
evalValue (ValNumber double) = show double
evalValue (ValIdent ident) = ident


-- Evaluates the TableNames into a more suitable structure
evalTableName :: TableName -> (FilePath,Ident)
evalTableName (TableAlias filePath ident) = (filePath,ident)



-- Evaluates all the conditions
-- evalConditions :: [Condition] -> TableName -> [JoinClause] -> IO [[Int]]
-- evalConditions [] _ _ = return []
-- evalConditions (c:cs) tbl jcs = do
--   evaledCond <- evalCondition c tbl jcs
--   rest <- evalConditions cs tbl jcs
--   return (evaledCond : rest)


-- -- Evaluates the Condition, and gives back the indexes (row numbers) that satify the condition
-- evalCondition :: Condition -> TableName -> [JoinClause] -> IO [Int]
-- evalCondition (Equals column val) tbl jcs = fmap (\x -> helperCondition (==) (zip [0..] x) val) (topLvlEvalColumn column tbl jcs)

-- evalCondition (NotEquals column val) tbl jcs = fmap (\x -> helperCondition (/=) (zip [0..] x) val) (topLvlEvalColumn column tbl jcs)

-- evalCondition (LessThan column val) tbl jcs = fmap (\x -> helperCondition (<) (zip [0..] x) val) (topLvlEvalColumn column tbl jcs)

-- evalCondition (GreaterThan column val) tbl jcs = fmap (\x -> helperCondition (>) (zip [0..] x) val) (topLvlEvalColumn column tbl jcs)

-- evalCondition (LessThanEq column val) tbl jcs = fmap (\x -> helperCondition (<=) (zip [0..] x) val) (topLvlEvalColumn column tbl jcs)

-- evalCondition (GreaterThanEq column val) tbl jcs = fmap (\x -> helperCondition (>=) (zip [0..] x) val) (topLvlEvalColumn column tbl jcs)

-- evalCondition (InList column vals) tbl jcs = fmap (\x -> helperInListCondition (zip [0..] x) vals) (topLvlEvalColumn column tbl jcs)

-- evalCondition (Between column val1 val2) tbl jcs = fmap (\x -> helperBetween (zip [0..] x) val1 val2) (topLvlEvalColumn column tbl jcs)






-- -- Applies the inList condition to all values in the column. Gives back a list of the indexes that satisfy the condition
-- helperInListCondition :: [(Int,String)] -> [Value] -> [Int]
-- helperInListCondition [] _ = []
-- helperInListCondition ((index,colStr):as) vs | inValueList colStr vs = index:helperInListCondition as vs
--                                              | otherwise = helperInListCondition as vs
--   where

--   inValueList :: String -> [Value] -> Bool
--   inValueList _ [] = False 
--   inValueList colStr (v:vs) | pureCompare colStr v (==) = True
--                             | otherwise = inValueList colStr vs





-- -- Applies the inBetween condition to all values in the column. Gives back a list of the indexes that satisfy the condition
-- helperBetween :: [(Int,String)] -> Value -> Value -> [Int]
-- helperBetween [] _ _ = []
-- helperBetween ((index,colStr):as) v1 v2 | comp1 && comp2 = index:helperBetween as v1 v2
--                                         | otherwise = helperBetween as v1 v2
  
--   where
--   comp1 = pureCompare colStr v1 (>=)
--   comp2 = pureCompare colStr v2 (<=)
    




-- -- Applies the given condition to all values in the column. Gives back a list of the indexes that satisfy the condition
-- helperCondition :: (forall a. Ord a => a -> a -> Bool) -> [(Int,String)] -> Value -> [Int]
-- helperCondition _ [] _ = []
-- helperCondition f ((index,colStr):as) value | pureCompare colStr value f = index:(helperCondition f as value)
--                                             | otherwise = helperCondition f as value
  

-- -- Gives back the non-monadic version of the compareVal ie. the pure boolean value
-- pureCompare ::  String -> Value -> (forall a. Ord a => a -> a -> Bool) -> Bool
-- pureCompare str val f = b
--   where
--   mb = compareVal str val f
--   b = case mb of
--     Nothing -> error "Non-matching types"
--     (Just bool) -> bool
  

-- -- Compares a string with a value within in the Maybe context
-- compareVal :: String -> Value -> (forall a. Ord a => a -> a -> Bool) -> Maybe Bool
-- compareVal colStr value f = 
--   case value of
--     ValInteger num -> do
--       parsed <- readMaybe colStr :: Maybe Int
--       return $ f parsed num
--     ValNumber num -> do
--       parsed <- readMaybe colStr :: Maybe Double
--       return $ f parsed num
--     ValString str ->
--       return $ f colStr str
--     ValIdent ident ->
--       return $ f colStr ident





-- -- :: OrderClause -> [(ordered row index, prev row index)]
-- evalOrderBy :: OrderClause -> TableName -> [JoinClause] -> IO [(Int,Int)]
-- evalOrderBy (OrderByAsc col) tbl jcs = fmap (\c -> helperOrderBy c True) (topLvlEvalColumn col tbl jcs)
-- evalOrderBy (OrderByDesc col) tbl jcs = fmap (\c -> helperOrderBy c False) (topLvlEvalColumn col tbl jcs)



-- -- Orders 
-- helperOrderBy :: [String] -> Bool -> [(Int,Int)]
-- helperOrderBy column isAsc | isAsc = zip [0..] [new | (new,_) <- (sortBy (comparing snd) (zip [0..] column))]
--                            | otherwise = zip [0..] [new | (new,_) <- (sortBy (comparing (Down . snd)) (zip [0..] column))]


-- -- :: LimitClause -> (Offset, Number of Rows)
-- evalLimit :: LimitClause -> (Int,Int)
-- evalLimit (Limit num) = (0,num)
-- evalLimit (LimitOffset offset num) = (offset,num)




-- -- evalColumn used by the higher lvl functions 
-- topLvlEvalColumn :: Column -> TableName -> [JoinClause] -> IO ([String])
-- topLvlEvalColumn col tbl jcs = do
--   let ident = (case col of
--                 (ColumnByIndex id _) -> id
--                 (ColumnByValue _ id) -> id)

--   let tbls = getTables (evalTableName tbl) jcs
--   let tblToBeEvaluated = findTable ident tbls

--   evalColumn col tblToBeEvaluated


-- -- Find the table belonging to the identity
-- findTable :: Ident -> [(FilePath,Ident)] -> TableName
-- findTable _ [] = error "Table of that identity does not exist"
-- findTable id ((fp,ident):ts) | id == ident = TableAlias fp ident
--                              | otherwise = findTable id ts

-- -- Evaluates a Column into a more suitable structure
-- evalColumn :: Column -> TableName -> IO ([String])
-- evalColumn (ColumnByIndex ident index) tbl = do
--   let (filePath,ident) = evalTableName tbl
--   getColumnByIndex filePath index
  