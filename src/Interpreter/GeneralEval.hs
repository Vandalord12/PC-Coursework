module Interpreter.GeneralEval where 
import Text.DslParser
import Interpreter.Csv 
import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)


type TableAliases = [(Ident,FilePath)]
-- I assume that only Identifier[Int] is the only way to access a column
-- data SelectStmt = Select (Maybe Distinct) Columns [TableName] (Maybe JoinClause) (Maybe [Condition]) (Maybe OrderClause) (Maybe LimitClause) (Maybe SelectStmt) (Maybe MergeMode)


evalJoin :: JoinClause -> SelectStmt -> IO [Bool]

-- Evaluates the Condition into a more suitable structure
evalCondition :: Condition -> SelectStmt -> IO [Bool]
evalCondition (Equals column val) stmt = fmap (map (\x -> x == val)) (evalColumn column stmt)

evalCondition (NotEquals column val) = fmap (map (\x -> x != val)) (evalColumn column stmt)

evalCondition (LessThan column val) = fmap (map (\x -> x < val)) (evalColumn column stmt)

evalCondition (GreaterThan column val) = fmap (map (\x -> x > val)) (evalColumn column stmt)

evalCondition (LessThanEq column val) = fmap (map (\x -> x <= val)) (evalColumn column stmt)

evalCondition (GreaterThanEq column val) = fmap (map (\x -> x >= val)) (evalColumn column stmt)

evalCondition (InList column vals) = fmap (map (\x -> elem x val)) (evalColumn column stmt)

-- This needs to be fixed
evalCondition (Like column pattern) = fmap (map (\x -> x == pattern)) (evalColumn column stmt)

evalCondition (Between column val1 val2) = fmap (map (\x -> (x >= val1 && x <= val2) )) (evalColumn column stmt)


-- Evaluates a Column into a more suitable structure
evalColumn :: Column -> SelectStmt -> IO ([String])
evalColumn (ColumnByIndex ident index) (Select _ _ tbls _ _ _ _ _ _) = do
  
  let filePath = findTableFilePath ident tableAliases
  case filePath of
    Nothing -> error "table alias does not exist"
    Just fp -> getColumnByIndex fp index
  where
    -- Finds the corresponding filePath fo the alias
    tableAliases = evalTableList tbls
    



-- Evaluates the TableNames into a more suitable structure
evalTableList :: [TableName] -> TableAliases
evalTableList [] = []
evalTableList ((TableAlias filePath ident):ts) = (ident,filePath):(evalTableList ts)



-- Finds the corresponding filePath for the table alias 
findTableFilePath :: Ident -> TableAliases -> Maybe FilePath
findTableFilePath _ [] = Nothing
findTableFilePath ident ((id,filePath):as) | ident == id = (Just filePath)
                                        | otherwise = findTableFilePath ident as