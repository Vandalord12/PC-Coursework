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