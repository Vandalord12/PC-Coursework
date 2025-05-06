{
module Text.DslParser where

import Text.Lexer
}

%name dslParser
%tokentype { Token } 
%error { parseError }


%token
-- This section defines the tokens for various SQL keywords, operators, and symbols --
-- The tokens are mapped to specific actions in the parser for proper DSL query parsing --
SELECT         {TokenSelect _}
FROM           {TokenFrom _}
WHERE          {TokenWhere _}
ORDER          {TokenOrder _}
BY             {TokenBy _}
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
LEFTMERGE      {TokenLeftMerge _ }
COALESCE       {TokenCoalEsce _}
INSERT         {TokenInsert _}
INTO           {TokenInto _}
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
CROSS          {TokenCross _}
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
AS              {TokenAs _}
-- Essential arthimitic Operators ------
"+"            {TokenPlus _}
"-"            {TokenMinus _}
"*"            {TokenMultiply _}
','            {TokenComma _ }
"/"            {TokenDivide _}
"%"            {TokenModulo _}
"."             {TokenDot _}
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
Number        { TokenNumber _ $$ }
Integer      {TokenInteger _ $$}
FilePath      {TokenPath _ $$}
String       { TokenString _ $$ }
Identifier   { TokenIdentifier _ $$ }

%left "+"
%left "-"
%left "*"
%left "/"
%left "="
%left "!="
%left "<"
%left ">"
%left "<="
%left ">="

%%



-------------------------------
-- SELECT Statement
-------------------------------


SelectStmt
    : SELECT OptDistinct Columns FROM TableName OptJoins OptWhere OptOrderBy OptLimit OptUnion { Select $2 $3 $5 $6 $7 $8 $9 $10}

-- Full grammar statement. Its been simplified for simplicity:
-- : SELECT OptDistinct ColumnList FROM TableList OptJoin OptWhere OptGroupBy OptHaving OptOrderBy OptLimit OptUnion    

OptDistinct: DISTINCT {Just Distinct} | {Nothing}

-- Column List
Columns:
    "*" {SelectAllColumns}
    | ColumnList {SelectColumns $1}


ColumnList:
    Column {[$1]}
    | Column ',' ColumnList {$1 : $3}

Column:
    Value AS Identifier {ColumnByValue $1 $3}
    | Identifier "[" Integer "]" {ColumnByIndex $1 $3}
    | COALESCE "(" Column ',' Column ")" AS Identifier { ColumnCoalesce $3 $5 $8}

TableName:
    FilePath AS Identifier {TableAlias $1 $3}
    --I trid the token FilePath in here and it did not work i got this error cql: oops something went wrong so i converted it to String it worked

-- Joins
OptJoins:
    JoinList {Just $1} | { Nothing }


JoinList:
    Join {[$1]}
    | Join JoinList {$1 : $2}

Join:
    JOIN TableName ON OnCondition {InnerJoin $2 $4}
    | INNER JOIN TableName ON OnCondition {InnerJoin $3 $5}
    | LEFT JOIN TableName ON OnCondition {LeftJoin $3 $5}
    | RIGHT JOIN TableName ON OnCondition {RightJoin $3 $5}
    | FULL JOIN TableName ON OnCondition {FullJoin $3 $5}
    | CROSS JOIN TableName ON OnCondition {CrossJoin $3 $5}

OnCondition:
    Column "=" Column             { OnColEquals $1 $3 }
    | Column "!=" Column            { OnColNotEquals $1 $3 }
    | Column "<" Column             { OnColLessThan $1 $3 }
    | Column ">" Column             { OnColGreaterThan $1 $3 }
    | Column "<=" Column            { OnColLessThanEq $1 $3 }
    | Column ">=" Column            { OnColGreaterThanEq $1 $3 }

-- Option Where
OptWhere: WHERE ConditionList { Just $2 } | { Nothing }


ConditionList: 
    Condition { [$1] } 
    | Condition AND ConditionList { $1 : $3 }
    | Condition OR ConditionList  { $1 : $3 }


Condition: 
    Column "=" Value             { Equals $1 $3 }
    | Column "!=" Value            { NotEquals $1 $3 }
    | Column "<" Value             { LessThan $1 $3 }
    | Column ">" Value             { GreaterThan $1 $3 }
    | Column "<=" Value            { LessThanEq $1 $3 }
    | Column ">=" Value            { GreaterThanEq $1 $3 }
    | Column "=" Column             { ColEquals $1 $3 }
    | Column "!=" Column            { ColNotEquals $1 $3 }
    | Column "<" Column             { ColLessThan $1 $3 }
    | Column ">" Column             { ColGreaterThan $1 $3 }
    | Column "<=" Column            { ColLessThanEq $1 $3 }
    | Column ">=" Column            { ColGreaterThanEq $1 $3 }
    | Column IN "(" ValueList ")"  { InList $1 $4 }
    | Column BETWEEN Value AND Value { Between $1 $3 $5 }
    --| Column LIKE String           { Like $1 $3 }
    



ValueList: 
    Value { [$1] } 
    | Value ',' ValueList { $1 : $3 }

Value: 
    String { ValString $1 }
    | Integer { ValInteger $1}
    | Number { ValNumber $1 } 
    | Identifier { ValIdent $1 }


OptOrderBy: 
    ORDER BY Column {Just (OrderByAsc $3)} -- Default value is in ascending order
    | ORDER BY Column ASC {Just (OrderByAsc $3)}
    | ORDER BY Column DESC {Just (OrderByDesc $3)}
    | ORDER ALL {Just (OrderAll)}
    | {Nothing}

OptLimit:
    LIMIT Integer {Just (Limit $2)}
    | LIMIT Integer OFFSET Integer {Just (LimitOffset $2 $4)}
    | {Nothing}

OptUnion:
    UNION SelectStmt {Just $2}
    | {Nothing}



{

parseError :: [Token] -> a
parseError = error "oops something went wrong"

type Ident = String

data SelectStmt = Select (Maybe Distinct) Columns TableName (Maybe [JoinClause]) (Maybe [Condition]) (Maybe OrderClause) (Maybe LimitClause) (Maybe SelectStmt) deriving (Show, Eq)
 
data Distinct = Distinct deriving (Show, Eq)

data Columns = SelectAllColumns | SelectColumns [Column] deriving (Show, Eq)

data Column = ColumnByValue Value Ident | ColumnByIndex Ident Int |  ColumnCoalesce Column Column Ident deriving (Show, Eq)

data TableName = TableAlias FilePath Ident deriving (Show, Eq)

-- Joins

data JoinClause 
    = InnerJoin TableName OnCondition
    | LeftJoin TableName OnCondition
    | RightJoin TableName OnCondition
    | FullJoin TableName OnCondition
    | CrossJoin TableName OnCondition
    deriving (Show, Eq)

data OnCondition
    = OnColEquals Column Column
    | OnColNotEquals Column Column
    | OnColLessThan Column Column
    | OnColGreaterThan Column Column
    | OnColLessThanEq Column Column
    | OnColGreaterThanEq Column Column
    deriving (Show, Eq)

data Condition
    = Equals Column Value
    | NotEquals Column Value
    | LessThan Column Value
    | GreaterThan Column Value
    | LessThanEq Column Value
    | GreaterThanEq Column Value
    | ColEquals Column Column
    | ColNotEquals Column Column
    | ColLessThan Column Column
    | ColGreaterThan Column Column
    | ColLessThanEq Column Column
    | ColGreaterThanEq Column Column
    | InList Column [Value]
    | Between Column Value Value
    deriving (Show, Eq)

data Value
    = ValString String
    | ValInteger Int
    | ValNumber Double
    | ValIdent Ident
    deriving (Show, Eq)

data OrderClause
    = OrderByAsc Column
    | OrderByDesc Column
    | OrderAll
    deriving (Show, Eq)

data LimitClause
    = Limit Int
    | LimitOffset Int Int
    deriving (Show, Eq)



}
