{
    module Lexer where 

    -- this import is checking whether a word is an alphabet and a number is a digit so it will help us in error checking.
    import Data.Char (isDigit, isAlpha)

}

-- this is responsible about the pattern stuff
%wrapper "posn"
$digit  = 0-9
$alpha  = [a-zA-Z]
$ident  = [$alpha _][$alpha $digit _]*


tokens :-
  $white+            ; 
  "--".*             ;   
  -- SQL Keywords
  "SELECT"          { \p s -> TokenSelect p }
  "FROM"            { \p s -> TokenFrom p }
  "WHERE"           { \p s -> TokenWhere p }
  "ORDER"           { \p s -> TokenOrder p }
  "BY"              { \p s -> TokenBy p }
  "ASC"             { \p s -> TokenAsc p }
  "DESC"            { \p s -> TokenDesc p }
  "GROUP"           { \p s -> TokenGroup p }
  "HAVING"          { \p s -> TokenHaving p }
  "LIMIT"           { \p s -> TokenLimit p }
  "OFFSET"          { \p s -> TokenOffset p }
  "DISTINCT"        { \p s -> TokenDistinct p }
    -- This chunck is responsible about Insert, Delete, Update
  "INSERT"          { \p s -> TokenInsert p }
  "INTO"            { \p s -> TokenInto p }
  "VALUES"          { \p s -> TokenValues p }
  "UPDATE"          { \p s -> TokenUpdate p }
  "SET"             { \p s -> TokenSet p }
  "DELETE"          { \p s -> TokenDelete p }
    -- Joins 
  "JOIN"            { \p s -> TokenJoin p }
  "INNER"           { \p s -> TokenInner p }
  "LEFT"            { \p s -> TokenLeft p }
  "RIGHT"           { \p s -> TokenRight p }
  "FULL"            { \p s -> TokenFull p }
  "OUTER"           { \p s -> TokenOuter p }
  "ON"              { \p s -> TokenOn p }
    -- Logical Operators
  "AND"            { \p s -> TokenAnd p }
  "OR"             { \p s -> TokenOr p }
  "NOT"            { \p s -> TokenNot p }
  "IN"             { \p s -> TokenIn p }
  "LIKE"           { \p s -> TokenLike p }

      -- Conditional Expressions
  "CASE"           { \p s -> TokenCase p }
  "WHEN"           { \p s -> TokenWhen p }
  "THEN"           { \p s -> TokenThen p }
  "ELSE"           { \p s -> TokenElse p }
  "END"            { \p s -> TokenEnd p }

  -- Essential arthimitic Operators
  "+"              { \p s -> TokenPlus p }
  "-"              { \p s -> TokenMinus p }
  "*"              { \p s -> TokenMultiply p }
  "/"              { \p s -> TokenDivide p }
  "%"              { \p s -> TokenModulo p }

  -- Comparison Operators
  "="              { \p s -> TokenEquals p }
  "!="             { \p s -> TokenNotEquals p }
  "<"              { \p s -> TokenLessThan p }
  ">"              { \p s -> TokenGreaterThan p }
  "<="             { \p s -> TokenLessThanEq p }
  ">="             { \p s -> TokenGreaterThanEq p }

<<<<<<< HEAD
=======
{
data Token =
  TokenSelect     |
  TokenFrom       |
  TokenWhere      |
  TokenOrder      |
  TokenBy         |
  TokenAsc        |
  TokenDesc       |
  TokenGroup      |
  TokenHaving     |
  TokenLimit      |
  TokenOffset     |
  TokenDistinct   |
    -- This chunck is responsible about Insert, Delete, Update
  TokenInsert    |
  TokenInto      |
  TokenValues    |
  TokenUpdate    |
  TokenSet       |
  TokenDelete    |
    -- Joins 
  TokenJoin      |
  TokenInner     |
  TokenLeft      |
  TokenRight     |
  TokenFull      |
  TokenOuter     |
  TokenOn        |
    -- Logical Operators
  TokenAnd       |
  TokenOr        |
  TokenNot       |
  TokenIn       |
  TokenLike      deriving(Eq,Show)
}
>>>>>>> dabf66b (data-token)

   
