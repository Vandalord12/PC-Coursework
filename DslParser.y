{
module DslParser where
import Lexer
}

%name DslParser 
%tokentype { Token } 
%error { parseError }


%token
-- This section defines the tokens for various SQL keywords, operators, and symbols --
-- The tokens are mapped to specific actions in the parser for proper DSL query parsing --
SELECT         {TokenSelect _}
FROM           {TokenFrom _}
WHERE          {TokenWhere }
ORDER          {TokenOrder _}
BY             {TokenBY _}
ASC            {TokenAsc _}
DESC           {TokenDesc _}
GROUP          {TokenGroup _}
HAVING         {TokenHaving _}
LIMIT          {TokenLimit _}
OFFSET         {TokenOffset _}
DISTINCT       {TokenDistinct _}
ALL            {TokenAll _}
ANY            {TokenAny _ }
BETWEEN        {TokenBetween _}
UNION          {TokenUnion _}
INTERSECT      {TokenIntersect _}
EXCEPT         {TokenExcept _}
INSERT         {TokenInsert _}
INTO          {TokenInto _}
VALUES         {TokenValues _}
UPDATE         {TokenUpdate _}
SET            {TokenSet _}
DELETE         {TokenDelete _}
JOIN           {TokenJoin _}
INNER          {TokenInner _}
LEFT           {TokenLeft _}
RIGHT          {TokenRight _}
FULL           {TokenFull _}
OUTER          {TokenOuter _}
CROSSJOIN      {TokenCrossJoin _}
ON             {TokenOn _ }
AND            {TokenAnd _ }
OR             {TokenOr _}
NOT            {TokenNot _}
IN             {TokenIn _}
LIKE           {TokenLike _}
CASE           {TokenCase _}
WHEN           {TokenWhen _}
THEN           {TokenThen _}
ELSE           {TokenElse _}
END            {TokenEnd _}
-- Essential arthimitic Operators ------
"+"            {TokenPlus _}
"-"            {TokenMinus _}
"*"            {TokenMultiply _}
','            {TokenComma _ }
"/"            {TokenDivide _}
"%"            {TokenModulo _}
-- Comparison Operators ----------------
"="            {TokenEquals _}
"!="           {TokenNotEquals _}
"<"            {TokenLessThan _}
">"            {TokenGreaterThan _}
"<="           {TokenLessThanEq _}
">="           {TokenGreaterThanEq _}
-- Parenthesis ------------------------
"("           {TokenLParen _}
")"           {TokenRParen _}
"["           {TokenLBracket _}
"]"           {TokenRBracket _}
"{"           {TokenLBrace _}
"}"           {TokenRBrace _}
-------Pattern Matching ------------------
Int          { TokenNumber _ $$ }
Filename     { TokenFilename _ $$ }
String       { TokenString _ $$ }
Identifier        { TokenIdentifier _ $$ }



%%

-------------------------------
-- SQL Statements
-------------------------------
Statement 
    : SelectStmt                 { SelectStatement $1 }
    | InsertStmt                 { InsertStatement $1 }
    | UpdateStmt                 { UpdateStatement $1 }
    | DeleteStmt                 { DeleteStatement $1 }
    | UnionStmt                  { UnionStatement $1 }

-------------------------------
-- SELECT Statement
-------------------------------
SelectStmt
    : SELECT OptDistinct ColumnList FROM TableList OptWhere OptGroupBy OptHaving OptOrderBy OptLimit
        { Select $2 $4 $5 $6 $7 $8 $9 }

InsertStmt:
    INSERT INTO Identifier VALUES MultStringList 
    | INSERT INTO Identifier "("ColumnList")" VALUES MultStringList
    

OptDistinct: DISTINCT {Just Distinct} | {Nothing}

ColumnList: 
    "*" {SelectAllColumns} 
    | Identifier {[$1]} 
    | Identifier ',' ColumnList {$1 : $3}

TableList: 
    "*" {SelectAllTables} 
    | Identifier {[$1]} 
    | Identifier ',' TableList {$1 : $3}

MultStringList: -- A 2D list: [[val1,val2],[val1,val2]]
    "(" StringList ")" {[$2]} 
    | "(" StringList ")" ',' MultStringList {$2 : $5}

StringList: 
    String {[$1]} 
    | String ',' StringList {$1 : $3}

OptWhere: WHERE ConditionList { Just $2 } | { Nothing }

ConditionList: Condition { [$1] } 
    | Condition AND ConditionList { $1 : $3 }
    | Condition OR ConditionList  { $1 : $3 }

Condition
    : Identifier "=" Value             { Equals $1 $3 }
    | Identifier "!=" Value            { NotEquals $1 $3 }
    | Identifier "<" Value             { LessThan $1 $3 }
    | Identifier ">" Value             { GreaterThan $1 $3 }
    | Identifier "<=" Value            { LessThanEq $1 $3 }
    | Identifier ">=" Value            { GreaterThanEq $1 $3 }
    | Identifier IN "(" ValueList ")"  { InList $1 $4 }
    | Identifier LIKE String           { Like $1 $3 }
    | Identifier BETWEEN Value AND Value { Between $1 $3 $5 }

ValueList: 
    Value { [$1] } 
    | Value ',' ValueList { $1 : $3 }

Value: 
    String { ValString $1 } 
    | Int { ValInt $1 } 
    | Identifier { ValIdent $1 }


OptOrderBy: 
    ORDER BY Identifier {Just OrderByAsc $3} -- Default value is in ascending order
    | ORDER BY Identifier ASC {Just OrderByAsc $3}
    | ORDER BY Identifier DESC {Just OrderByDesc $3}
    | {Nothing}



-- OptGroupBy
--     : GROUP BY ColumnList         { Just $3 }
--     |                             { Nothing }

-- OptHaving
--     : HAVING ConditionList        { Just $2 }
--     |                             { Nothing }


--     : ORDER BY Ident OptDirection { Just (OrderBy $3 $4) }
--     |                             { Nothing }

-- OptDirection
--     : ASC                         { Asc }
--     | DESC                        { Desc }
--     |                             { Asc }  -- Default is Ascending

-- OptLimit
--     : LIMIT Int                   { Just (Limit $2) }
--     |                             { Nothing }

-- -------------------------------
-- -- INSERT Statement
-- -------------------------------
-- InsertStmt
--     : INSERT INTO Ident "(" ColumnList ")" VALUES "(" ValueList ")"
--         { Insert $3 $5 $9 }

-- -------------------------------
-- -- UPDATE Statement
-- -------------------------------
-- UpdateStmt
--     : UPDATE Ident SET AssignList OptWhere
--         { Update $2 $4 $5 }

-- AssignList
--     : Ident "=" Value             { [(Assign $1 $3)] }
--     | AssignList ',' Ident "=" Value { $1 ++ [(Assign $3 $5)] }

-- -------------------------------
-- -- DELETE Statement
-- -------------------------------
-- DeleteStmt
--     : DELETE FROM Ident OptWhere
--         { Delete $3 $4 }

-- -------------------------------
-- -- JOIN Clauses
-- -------------------------------
-- JoinStmt
--     : TableList JoinClause        { JoinTables $1 $2 }

-- JoinClause
--     : JOIN Ident ON Condition     { Join $2 $4 }
--     | LEFT JOIN Ident ON Condition { LeftJoin $3 $5 }
--     | RIGHT JOIN Ident ON Condition { RightJoin $3 $5 }
--     | FULL JOIN Ident ON Condition { FullJoin $3 $5 }
--     | CROSSJOIN Ident             { CrossJoin $2 }

-- -------------------------------
-- -- UNION, INTERSECT, and EXCEPT
-- -------------------------------
-- UnionStmt
--     : SelectStmt UNION SelectStmt    { Union $1 $3 }
--     | SelectStmt INTERSECT SelectStmt { Intersect $1 $3 }
--     | SelectStmt EXCEPT SelectStmt    { Except $1 $3 }

-- -------------------------------
-- -- Expressions and Parentheses
-- -------------------------------
-- Expr
--     : Value                        { ValExpr $1 }
--     | Expr "+" Expr                { Add $1 $3 }
--     | Expr "-" Expr                { Subtract $1 $3 }
--     | Expr "*" Expr                { Multiply $1 $3 }
--     | Expr "/" Expr                { Divide $1 $3 }
--     | "(" Expr ")"                 { $2 }

-- -------------------------------
-- -- Helper Productions
-- -------------------------------
-- Ident
--     : TokenIdentifier _ $$         { Ident $$ }

-- String
--     : TokenString _ $$             { $$ }

-- Int
--     : TokenNumber _ $$             { $$ }

-- %%
-- -------------------------- GRAMER ----------------------
-- --------------------------------------------------------
-- SelectQuery : SELECT "*" FROM TableList                          { SelectStarColumns $4 }
--             | SELECT ColumnList FROM TableList                   { SelectColumns $2 $4 }
--             | SELECT "*" FROM TableList WHERE ConditionList       { SelectStarColumnsWhere $4 $6 }
--             | SELECT ColumnList FROM TableList WHERE ConditionList { SelectColumnsWhere $2 $4 $6 }
--             | SELECT "*" FROM TableList ORDER BY Ident Direction   { SelectStarColumnsOrder $4 (OrderBy $6 $7) }
--             | SELECT ColumnList FROM TableList ORDER BY Ident Direction { SelectColumnsOrder $2 $4 (OrderBy $6 $7) }
--             | SELECT "*" FROM TableList WHERE ConditionList ORDER BY Ident Direction { SelectStarColumnsWhereOrder $4 $6 (OrderBy $8 $9) }
--             | SELECT ColumnList FROM TableList WHERE ConditionList ORDER BY Ident Direction { SelectColumnsWhereOrder $2 $4 $6 (OrderBy $8 $9) }



-- ColumnList :
--     Ident                           { [$1] }
--   | Ident ',' ColumnList            { $1 : $3 }

-- TableList :
--     Ident                        { [$1] }
--   | Ident ',' TableList          { $1 : $3 }

-- ConditionList :
--     Condition                       { [$1] }
--   | Condition AND ConditionList     { $1 : $3 }

-- Condition :
--     Ident "=" Value                 { Equals $1 $3 }
--   | Ident "!=" Value                { NotEq $1 $3 }
--   | Ident "<" Value                 { LessThan $1 $3 }
--   | Ident ">" Value                 { GreaterThan $1 $3 }
--   | Ident "<=" Value                { LessThanEq $1 $3 }
--   | Ident ">=" Value                { GreaterThanEq $1 $3 }

-- Value :
--     String                         { ValString $1 }
--   | Int                            { ValNumber $1 }

-- Direction :
--     ASC   { Asc }
--   | DESC  { Desc }

-- ArithmeticOperations: "+" {Add}
--                     | "-" {Minus}
--                     | "*" {Multiply}
--                     | ',' {Comma}
--                     | "/" {Divide}
--                     | "%" {Modulo}

-- ComparsionOperators: "=" { Equals}
--                     |"!=" {NotEq}
--                     | "<" {LessThan}
--                     | ">" {GreaterThan}
--                     | "<=" {LessThanEq}
--                     | ">=" {GreaterThanEq}
                  
                  

-- -------------------------- DATA TYPES FOR THE GRAMMER ----------------------
-- ----------------------------------------------------------------------------
-- {
-- type ColumnName = String
-- type TableName = String

-- --condition for comparison stuff---
-- data Condition
--   = Equals ColumnName Value
--   | NotEq ColumnName Value
--   | LessThan ColumnName Value
--   | GreaterThan ColumnName Value
--   | LessThanEq ColumnName Value
--   | GreaterThanEq ColumnName Value
--   deriving (Show, Eq)

-- -- types for the value of the comparison---
-- data Value
--   = ValString String
--   | ValNumber Double
--   deriving (Show, Eq)

-- data SelectQuery
--   = SelectStarColumns [TableName]                             
--   | SelectColumns [ColumnName] [TableName]                    
--   | SelectStarColumnsWhere [TableName] [Condition]           
--   | SelectColumnsWhere [ColumnName] [TableName] [Condition]   
--   | SelectStarColumnsOrder [TableName] OrderBy
--   | SelectColumnsOrder [ColumnName] [TableName] OrderBy
--   | SelectStarColumnsWhereOrder [TableName] [Condition] OrderBy
--   | SelectColumnsWhereOrder [ColumnName] [TableName] [Condition] OrderBy
--   deriving (Show, Eq)

-- data OrderDirection = Asc | Desc
--   deriving (Show, Eq)

-- data OrderBy = OrderBy ColumnName OrderDirection
--   deriving (Show, Eq)

-- data Star = Star deriving (Show, Eq)

-- data ArithmeticOperations = Add 
--                           | Minus
--                           |Multiply
--                           |Comma
--                           |Divide
--                           |Modulo
--                           deriving (Show, Eq)

-- data ComparsionOperators = Equals
--                          | NotEq
--                          | LessThan
--                          | GreaterThan
--                          | LessThanEq
--                          | GreaterThanEq
--                          deriving (Show, Eq)
                  
-- }