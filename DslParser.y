{
module DslParser (parserDsl) where
import Lexer
}

%name parserDsl 
%tokentype { Token } 
%error { parseError }
%token
-- This section defines the tokens for various SQL keywords, operators, and symbols --
-- The tokens are mapped to specific actions in the parser for proper DSL query parsing --
SELECT         {TokenSelect _}
FROM           {TokenFrom _}
WHERE          {TokenWhere _ }
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
Ident        { TokenIdentifier _ $$ }


%left OR
%left AND
%nonassoc "=" "!=" "<" ">" "<=" ">="
%left "+" "-"
%left "*" "/" "%"
%%
-------------------------- GRAMER ----------------------
--------------------------------------------------------
SelectQuery : SELECT "*" FROM TableList                          { SelectStarColumns $4 }
            | SELECT ColumnList FROM TableList                   { SelectColumns $2 $4 }
            | SELECT "*" FROM TableList WHERE ConditionList       { SelectStarColumnsWhere $4 $6 }
            | SELECT ColumnList FROM TableList WHERE ConditionList { SelectColumnsWhere $2 $4 $6 }
           -- | SELECT "*" FROM TableList ORDER BY Ident Direction   { SelectStarColumnsOrder $4 (OrderBy $6 $7) }
           -- | SELECT ColumnList FROM TableList ORDER BY Ident Direction { SelectColumnsOrder $2 $4 (OrderBy $6 $7) }
           -- | SELECT "*" FROM TableList WHERE ConditionList ORDER BY Ident Direction { SelectStarColumnsWhereOrder $4 $6 (OrderBy $8 $9) }
           -- | SELECT ColumnList FROM TableList WHERE ConditionList ORDER BY Ident Direction { SelectColumnsWhereOrder $2 $4 $6 (OrderBy $8 $9) }



ColumnList :
    Ident                           { [$1] }
  | Ident ',' ColumnList            { $1 : $3 }

TableList :
    Ident                        { [$1] }
  | Ident ',' TableList          { $1 : $3 }

ConditionList :
    Condition                       { [$1] }
  | Condition AND ConditionList     { $1 : $3 }

Condition :
    Ident "=" Value                 { Equals $1 $3 }
  | Ident "!=" Value                { NotEq $1 $3 }
  | Ident "<" Value                 { LessThan $1 $3 }
  | Ident ">" Value                 { GreaterThan $1 $3 }
  | Ident "<=" Value                { LessThanEq $1 $3 }
  | Ident ">=" Value                { GreaterThanEq $1 $3 }

Value :
    String                         { ValString $1 }
  | Int                            { ValNumber $1 }

--Direction :
 --   ASC   { Asc }
  --| DESC  { Desc }

ArithmeticOperations: "+" {Add}
                    | "-" {Minus}
                    | "*" {Multiply}
                    | ',' {Comma}
                    | "/" {Divide}
                    | "%" {Modulo}

ComparsionOperators: "=" { OpEquals}
                    |"!=" {OpNotEq}
                    | "<" {OpLessThan}
                    | ">" {OpGreaterThan}
                    | "<=" {OpLessThanEq}
                    | ">=" {OpGreaterThanEq}
                  
                  

-------------------------- DATA TYPES FOR THE GRAMMER ----------------------
----------------------------------------------------------------------------
{

parseError :: [Token] -> a
parseError [] = error "Unknown Parse Error" 
parseError (t:ts) = error ("Parse error at line:column " ++ (tokenPosn t) ++ " : " ++ (show ts))

type ColumnName = String
type TableName = String

--condition for comparison stuff---
data Condition
  = Equals ColumnName Value
  | NotEq ColumnName Value
  | LessThan ColumnName Value
  | GreaterThan ColumnName Value
  | LessThanEq ColumnName Value
  | GreaterThanEq ColumnName Value
  deriving (Show, Eq)

-- types for the value of the comparison---
data Value
  = ValString String
  | ValNumber Double
  deriving (Show, Eq)

data SelectQuery
  = SelectStarColumns [TableName]                             
  | SelectColumns [ColumnName] [TableName]                    
  | SelectStarColumnsWhere [TableName] [Condition]           
  | SelectColumnsWhere [ColumnName] [TableName] [Condition]   
 -- | SelectStarColumnsOrder [TableName] OrderBy --
 -- | SelectColumnsOrder [ColumnName] [TableName] OrderBy --
 -- | SelectStarColumnsWhereOrder [TableName] [Condition] OrderBy --
 -- | SelectColumnsWhereOrder [ColumnName] [TableName] [Condition] OrderBy --
  deriving (Show, Eq)

-- data OrderDirection = Asc | Desc
--  deriving (Show, Eq)

-- data OrderBy = OrderBy ColumnName OrderDirection
 -- deriving (Show, Eq)

data Star = Star deriving (Show, Eq)

data ArithmeticOperations = Add 
                          | Minus
                          |Multiply
                          |Comma
                          |Divide
                          |Modulo
                          deriving (Show, Eq)

data ComparsionOperators = OpEquals
                         | OpNotEq
                         | OpLessThan
                         | OpGreaterThan
                         | OpLessThanEq
                         | OpGreaterThanEq
                         deriving (Show, Eq)
                  
}