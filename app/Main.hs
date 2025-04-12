module Main (
    main
  ) where

import Text.Lexer (alexScanTokens)
import Text.DslParser (dslParser)
import Data.Maybe (isNothing, isJust)
import Interpreter.Csv

--main :: IO ()
main = do
  code <- readFile "resources/code.txt"
  tokens <- toIO (alexScanTokens code)
  ast <- toIO (dslParser tokens)
  putStrLn (show ast)
  return ast

toIO :: a -> IO a
toIO x = return x


-- OptDistinct: DISTINCT {Just Distinct} | {Nothing}

-- -- Column List
-- Columns:
--     "*" {SelectAllColumns}
--     | ColumnList {SelectColumns $1}


-- ColumnList:
--     Column {[$1]}
--     | Column ',' ColumnList {$1 : $3}

-- Column:
--     Identifier {ColumnIdent $1}
--     | Value AS Identifier {ColumnByValue $1 $3}
--     | Identifier "[" Integer "]" {ColumnByIndex $1 $3}
--     | Identifier "." Identifier {ColumnByName $1 $3}
type Map = [(a,a)]

getColumnByIndex :: String -> Int -> [String]
getColumnByIndex fileName index = do
  contents <- readFile 
getRowByIndex :: String -> Int -> [String]
getSpecificValue :: String -> Int -> Int -> [String]

columnFinder :: Column -> [String]
columnFiner (ColumnByIndex tbl num) = do 

-- -- Table List
-- TableList: 
--     TableName {[$1]} 
--     | TableName ',' TableList {$1 : $3}

-- TableName:
--     FilePath AS Identifier {TableAlias $1 $3}

-- -- Joins

-- OptJoin:
--     JOIN Identifier ON OnCondition {Just (InnerJoin $2 $4)}
--     | INNER JOIN Identifier ON OnCondition {Just (InnerJoin $3 $5)}
--     | LEFT JOIN Identifier ON OnCondition {Just (LeftJoin $3 $5)}
--     | RIGHT JOIN Identifier ON OnCondition {Just (RightJoin $3 $5)}
--     | FULL JOIN Identifier ON OnCondition {Just (FullJoin $3 $5)}
--     | CROSS JOIN Identifier {Just (CrossJoin $3)}
--     | {Nothing}

-- OnCondition:
--     Column "=" Column             { ColEquals $1 $3 }
--     | Column "!=" Column            { ColNotEquals $1 $3 }
--     | Column "<" Column             { ColLessThan $1 $3 }
--     | Column ">" Column             { ColGreaterThan $1 $3 }
--     | Column "<=" Column            { ColLessThanEq $1 $3 }
--     | Column ">=" Column            { ColGreaterThanEq $1 $3 }

-- -- Option Where
-- OptWhere: WHERE ConditionList { Just $2 } | { Nothing }


-- ConditionList: 
--     Condition { [$1] } 
--     | Condition AND ConditionList { $1 : $3 }
--     | Condition OR ConditionList  { $1 : $3 }


-- Condition: 
--     Column "=" Value             { Equals $1 $3 }
--     | Column "!=" Value            { NotEquals $1 $3 }
--     | Column "<" Value             { LessThan $1 $3 }
--     | Column ">" Value             { GreaterThan $1 $3 }
--     | Column "<=" Value            { LessThanEq $1 $3 }
--     | Column ">=" Value            { GreaterThanEq $1 $3 }
--     | Column IN "(" ValueList ")"  { InList $1 $4 }
--     | Column LIKE String           { Like $1 $3 }
--     | Column BETWEEN Value AND Value { Between $1 $3 $5 }



-- ValueList: 
--     Value { [$1] } 
--     | Value ',' ValueList { $1 : $3 }

-- Value: 
--     String { ValString $1 }
--     | Integer { ValInteger $1}
--     | Number { ValNumber $1 } 
--     | Identifier { ValIdent $1 }


-- OptOrderBy: 
--     ORDER BY Column {Just (OrderByAsc $3)} -- Default value is in ascending order
--     | ORDER BY Column ASC {Just (OrderByAsc $3)}
--     | ORDER BY Column DESC {Just (OrderByDesc $3)}
--     | {Nothing}

-- OptLimit:
--     LIMIT Integer {Just (Limit $2)}
--     | LIMIT Integer OFFSET Integer {Just (LimitOffset $2 $4)}
--     | {Nothing}

-- OptUnion:
--     UNION SelectStmt {Just $2}
--     | {Nothing}






    