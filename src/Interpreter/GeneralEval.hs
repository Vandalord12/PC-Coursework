{-# LANGUAGE RankNTypes #-}

module Interpreter.GeneralEval where 
import Text.DslParser
import Interpreter.Csv 
import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)
import Data.List
import Data.Ord 

-- type TableAlias = (FilePath, Ident)
type TablesData = [((String,FilePath),Table)]
-- I assume that only Identifier[Int] is the only way to access a column
-- data SelectStmt = Select (Maybe Distinct) Columns TableName (Maybe JoinClause) (Maybe [Condition]) (Maybe OrderClause) (Maybe LimitClause) (Maybe SelectStmt) (Maybe MergeMode)



--evalSelectStmt :: SelectStmt
evalSelectStmt (Select _ cols tbl joins conds order limit _ _) = do
  

  let jcs = (case joins of
              Just js -> js
              Nothing -> [])

  columns <- evalColumns cols tbl jcs
  -- distinctFilter <- case dist of
  --   Just _ -> nub columns
  --   Nothing -> []
  let rows = transpose columns 

  joinFilter <- case joins of
    Just js -> evalJoins tbl js js
    Nothing -> return []

  condsFilter <- case conds of
    Just cs -> evalConditions cs tbl jcs
    Nothing -> return []

  orderFilter <- case order of 
    Just ord -> evalOrderBy ord tbl jcs
    Nothing -> return []
  
  let limitFilter = (case limit of
                      Just lim -> evalLimit lim
                      Nothing -> (0,1000))
  
  return jcs


helperSelectStmt :: [[String]] -> [[(Int, Int)]] -> [[Int]] -> [(Int,Int)] -> (Int,Int) -> [[String]]
helperSelectStmt rows joins conds order _ = 
  where
  j = map (fst) joins



-- Evaluates all the columns
evalColumns :: Columns -> TableName -> [JoinClause] -> IO [[String]]
evalColumns cols tbl jcs = case cols of
    (SelectAllColumns) -> allColumns tbl
    (SelectColumns cs) -> selectedColumns cs tbl jcs


-- Helper function to evalColumns
allColumns :: TableName -> IO [[String]]
allColumns tbl = do

  let (filePath,_) = evalTableName tbl

  allCols <- readCSV filePath
  return allCols

-- Helper function to evalColumns
selectedColumns :: [Column] -> TableName -> [JoinClause] -> IO [[String]]
selectedColumns [] _ _ = return []
selectedColumns (col:cols) tbl jcs = do
  evaledColumn <- topLvlEvalColumn col tbl jcs
  rest <- selectedColumns cols tbl jcs
  return (evaledColumn : rest)
  

-- Evaluate all the join clauses
evalJoins :: TableName -> [JoinClause] -> [JoinClause] -> IO [[(Int, Int)]]
evaledJoin _ [] _ = return []
evalJoins tbl (j:js) jcs = do
  evaledJoin <- evalJoin j tbl jcs
  rest <- evalJoins tbl js jcs
  return (evaledJoin : rest)


-- Makes evaluating the JoinClause easier
extractJoin :: JoinClause -> (String, TableName, OnCondition)
extractJoin jc = (kind, ta, cond)
  where
  (kind, ta, cond) = case jc of
    InnerJoin t c -> ("Inner", t, c)
    LeftJoin t c -> ("Left", t, c)
    RightJoin t c -> ("Right", t, c)
    FullJoin t c -> ("Full", t, c)
    CrossJoin t -> ("Cross", t, (ColEquals (ColumnByIndex "null" 0) (ColumnByIndex "null" 0))) -- This needs work
  

-- Evaluates Join and gives back the rows that match in both tables as their row number pairs
evalJoin :: JoinClause -> TableName -> [JoinClause] -> IO [(Int, Int)]
evalJoin jc tbl1 jcs = do

  let (kind, tbl2, cond) = extractJoin jc

  let (predicate, (c1, c2)) = evalOnCondition cond
  col1 <- topLvlEvalColumn c1 tbl1 jcs
  col2 <- topLvlEvalColumn c2 tbl1 jcs

  allCol1 <- allColumns tbl1
  allCol2 <- allColumns tbl2

  let matchingPairs = case kind of
        "Inner"  -> helperJoin col1 col2 predicate "Inner"
        "Left"   -> helperJoin col1 col2 predicate "Left"
        "Right"  -> helperJoin col1 col2 predicate "Right"
        "Full"   -> helperJoin col1 col2 predicate "Full"
        "Cross"  -> crossJoin (zip [0..] (head allCol1)) (zip [0..] (head allCol2))
  return matchingPairs


-- Evaluates the OnCondition into a more suitable structure
evalOnCondition :: OnCondition -> ((String -> String -> Bool),(Column, Column))
evalOnCondition (ColEquals col1 col2) = ((==),(col1,col2))
evalOnCondition (ColNotEquals col1 col2) = ((/=),(col1,col2))
evalOnCondition (ColLessThan col1 col2) = ((<),(col1,col2))
evalOnCondition (ColGreaterThan col1 col2) = ((>),(col1,col2))
evalOnCondition (ColLessThanEq col1 col2) = ((<=),(col1,col2))
evalOnCondition (ColGreaterThanEq col1 col2) = ((>=),(col1,col2))



-- Does the meaty work of doing the actual join computation 
helperJoin :: [String] -> [String] -> (String -> String -> Bool) -> String -> [(Int, Int)]
helperJoin rawls rawrs f "Inner" = [ (i, j) | (i, x) <- zip [0..] rawls, (j, y) <- zip [0..] rawrs, f x y]
helperJoin rawls rawrs f "Left" = leftJoin (zip [0..] rawls) (zip [0..] rawrs) f
helperJoin rawls rawrs f "Right" = rightJoin (zip [0..] rawls) (zip [0..] rawrs) f
helperJoin rawls rawrs f "Full" = fullJoin (zip [0..] rawls) (zip [0..] rawrs) f
helperJoin rawls rawrs _ "Cross" = crossJoin (zip [0..] rawls) (zip [0..] rawrs)




-- Computes which rows in first csv matchs to which rows in the second csv, and gives back their indexes in pairs
leftJoin :: [(Int, String)] -> [(Int, String)] -> (String -> String -> Bool) -> [(Int,Int)]
leftJoin [] _ _ = []
leftJoin ((index,val):ls) rs f = [(index, check val val2 i f) | (i, val2) <- rs] ++ (leftJoin ls rs f)


leftJoin :: Table -> Table -> JoinClause ->  (String -> String -> Bool) -> Table
rightJoin :: Table -> Table -> JoinClause ->  (String -> String -> Bool) -> Table
fullJoin :: Table -> Table -> JoinClause ->  (String -> String -> Bool) -> Table
crossJoin :: Table -> Table -> JoinClause ->  (String -> String -> Bool) -> Table

-- Computes which rows in first csv matchs to which rows in the second csv, and gives back their indexes in pairs
rightJoin ::[(Int, String)] -> [(Int, String)] -> (String -> String -> Bool) -> [(Int,Int)]
rightJoin _ [] _ = []
rightJoin ls ((index,val):rs) f = [(check val val2 i f, index) | (i, val2) <- rs] ++ (rightJoin ls rs f)


-- Computes which rows in first csv matchs to which rows in the second csv, and gives back their indexes in pairs
fullJoin :: [(Int, String)] -> [(Int, String)] -> (String -> String -> Bool) -> [(Int,Int)]
fullJoin ls rs f = nub $ (leftJoin ls rs f) ++ (rightJoin ls rs f)


-- Computes which rows in first csv matchs to which rows in the second csv, and gives back their indexes in pairs
crossJoin :: [(Int, String)] -> [(Int, String)] -> [(Int,Int)]
crossJoin ls rs = [(i1,i2) | (i1,_) <- ls, (i2,_) <- rs]


-- Custom check to see if two values satisfy a condition
check :: String -> String -> Int -> (String -> String -> Bool) -> Int
check val1 val2 index f | f val1 val2 = index
                        | otherwise = -1







-- Evaluates all the conditions how it works:
-- Filters rows from the Cartesian product that satisfy all conditions.
evalConditions :: TablesData -> [Condition] -> Table
evalConditions tablesData conds =
  filter (\row -> all (\cond -> rowSatisfies row cond tablesData) conds) fullRows
  where
    allTables = map snd tablesData
    fullRows  = cartesianProduct allTables

-- Generates all row combinations from multiple tables
cartesianProduct :: [Table] -> Table
cartesianProduct [] = [[]]
cartesianProduct (t:ts) = [row1 ++ row2 | row1 <- t, row2 <- cartesianProduct ts]


-- Checks whether a combined row satisfies a given condition, using correct table slices by alias.
rowSatisfies :: Row -> Condition -> TablesData -> Bool
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


-- Splits a flat cartesian row into slices corresponding to each table's alias.
splitRowByTables :: Row -> TablesData -> [(Ident, Row)]
splitRowByTables row tables = go row tables []
  where
    go [] [] acc = reverse acc
    go r (((alias, _), t):ts) acc =
      let n = if null t then 0 else length (head t)
          (chunk, rest) = splitAt n r
      in go rest ts ((alias, chunk):acc)
    go _ _ _ = error "Row and tables mismatch"

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


-- -- keep the left side 
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


-- Applies the inList condition to all values in the column. Gives back a list of the indexes that satisfy the condition
helperInListCondition :: [(Int,String)] -> [Value] -> [Int]
helperInListCondition [] _ = []
helperInListCondition ((index,colStr):as) vs | inValueList colStr vs = index:helperInListCondition as vs
                                             | otherwise = helperInListCondition as vs
  where

  inValueList :: String -> [Value] -> Bool
  inValueList _ [] = False 
  inValueList colStr (v:vs) | pureCompare colStr v (==) = True
                            | otherwise = inValueList colStr vs





-- Applies the inBetween condition to all values in the column. Gives back a list of the indexes that satisfy the condition
helperBetween :: [(Int,String)] -> Value -> Value -> [Int]
helperBetween [] _ _ = []
helperBetween ((index,colStr):as) v1 v2 | comp1 && comp2 = index:helperBetween as v1 v2
                                        | otherwise = helperBetween as v1 v2
  
  where
  comp1 = pureCompare colStr v1 (>=)
  comp2 = pureCompare colStr v2 (<=)
    




-- Applies the given condition to all values in the column. Gives back a list of the indexes that satisfy the condition
helperCondition :: (forall a. Ord a => a -> a -> Bool) -> [(Int,String)] -> Value -> [Int]
helperCondition _ [] _ = []
helperCondition f ((index,colStr):as) value | pureCompare colStr value f = index:(helperCondition f as value)
                                            | otherwise = helperCondition f as value
  

-- Gives back the non-monadic version of the compareVal ie. the pure boolean value
pureCompare ::  String -> Value -> (forall a. Ord a => a -> a -> Bool) -> Bool
pureCompare str val f = b
  where
  mb = compareVal str val f
  b = case mb of
    Nothing -> error "Non-matching types"
    (Just bool) -> bool
  


-- :: OrderClause -> [(ordered row index, prev row index)]
-- evalOrderBy :: OrderClause -> TableName -> [JoinClause] -> IO [(Int,Int)]
-- evalOrderBy (OrderByAsc col) tbl jcs = fmap (\c -> helperOrderBy c True) (topLvlEvalColumn col tbl jcs)
-- evalOrderBy (OrderByDesc col) tbl jcs = fmap (\c -> helperOrderBy c False) (topLvlEvalColumn col tbl jcs)

-- Sorts the table based on the given OrderClause using column values extracted from the appropriate tables.
evalOrderBy :: OrderClause -> TablesData -> Table -> Table
evalOrderBy orderClause tablesData table =
  let
    extractKey = buildSortKeyExtractor orderClause tablesData
  in
    case orderClause of
      OrderByAsc _  -> sortBy (compareRows extractKey) table
      OrderByDesc _ -> sortBy (flip $ compareRows extractKey) table

-- Builds a function that extracts the sort key a String from a row based on the given OrderClause and table structure.
buildSortKeyExtractor :: OrderClause -> TablesData -> (Row -> String)
buildSortKeyExtractor clause tables =
  case clause of
    OrderByAsc col  -> \row -> extractValueFromColumn col row tables
    OrderByDesc col -> \row -> extractValueFromColumn col row tables

-- Retrieves the value from the specified column in a row by looking up the corresponding table alias and index.
extractValueFromColumn :: Column -> Row -> TablesData -> String
extractValueFromColumn (ColumnByIndex alias idx) row tables =
  case lookup alias (splitRowByTables row tables) of
    Just subRow -> safeIndex idx subRow
    Nothing     -> error $ "Alias '" ++ alias ++ "' not found."
extractValueFromColumn (ColumnByValue val _) _ _ = evalValue val

-- this compares the rows 
compareRows :: (Row -> String) -> Row -> Row -> Ordering
compareRows keyFn r1 r2 = compare (keyFn r1) (keyFn r2)






-- Orders 
-- helperOrderBy :: [String] -> Bool -> [(Int,Int)]
-- helperOrderBy column isAsc | isAsc = zip [0..] [new | (new,_) <- (sortBy (comparing snd) (zip [0..] column))]
--                            | otherwise = zip [0..] [new | (new,_) <- (sortBy (comparing (Down . snd)) (zip [0..] column))]



-- :: LimitClause -> (Offset, Number of Rows)

-- Applies a LIMIT clause to return a subset of rows using take, optionally skipping a number of rows with OFFSET using drop.
evalLimit :: LimitClause -> Table -> Table
evalLimit (Limit n) table = take n table
evalLimit (LimitOffset offset n) table = take n (drop offset table)



-- Evaluates the Value into a more suitable structure
evalValue :: Value -> String
evalValue (ValString str) = str
evalValue (ValInteger int) = show int
evalValue (ValNumber double) = show double
evalValue (ValIdent ident) = ident





-- Evaluates the TableNames into a more suitable structure
evalTableName :: TableName -> TableAlias
evalTableName (TableAlias filePath ident) = (filePath,ident)


-- Gives all the tables referenced
getTables :: TableAlias -> [JoinClause] -> [(FilePath,Ident)]
getTables _ [] = []
getTables (fp1,id1) (jc:jcs) = tbla:(getTables (fp1,id1) jcs)
  where
    (_, t, _) = extractJoin jc
    (fp2,id2) = evalTableName t

    tbla = case (fp2,id2) of
      (fp1, _) -> error "Referencing the same file twice bad"
      (_, id1) -> error "Cannot have the same id for two or more files"
      (f,i) -> (f,i)



-- evalColumn used by the higher lvl functions 
topLvlEvalColumn :: Column -> TableName -> [JoinClause] -> IO ([String])
topLvlEvalColumn col tbl jcs = do
  let ident = (case col of
                (ColumnByIndex id _) -> id
                (ColumnByValue _ id) -> id)

  let tbls = getTables (evalTableName tbl) jcs
  let tblToBeEvaluated = findTable ident tbls

  evalColumn col tblToBeEvaluated


-- Find the table belonging to the identity
findTable :: Ident -> [(FilePath,Ident)] -> TableName
findTable _ [] = error "Table of that identity does not exist"
findTable id ((fp,ident):ts) | id == ident = TableAlias fp ident
                             | otherwise = findTable id ts




-- Evaluates a Column into a more suitable structure
evalColumn :: Column -> TableName -> IO ([String])
evalColumn (ColumnByIndex ident index) tbl = do
  let (filePath,ident) = evalTableName tbl
  getColumnByIndex filePath index
  