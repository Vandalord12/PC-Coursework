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
   SELECT           { \p s -> TokenSelect p }
   FROM             { \p s -> TokenFrom p }
   WHERE            { \p s -> TokenWhere p }
   ORDER            { \p s -> TokenOrder p }
   BY               { \p s -> TokenBy p }
   ASC              { \p s -> TokenAsc p }
   DESC             { \p s -> TokenDesc p }
   GROUP            { \p s -> TokenGroup p }
   HAVING           { \p s -> TokenHaving p }
   LIMIT            { \p s -> TokenLimit p }
   OFFSET           { \p s -> TokenOffset p }
   DISTINCT         { \p s -> TokenDistinct p }
   ALL              { \p s -> TokenAll p }
   ANY              { \p s -> TokenAny p }
   BETWEEN          { \p s -> TokenBetween p }
   UNION            { \p s -> TokenUnion p }
   INTERSECT        { \p s -> TokenIntersect p }
   EXCEPT           { \p s -> TokenExcept p }
    -- This chunck is responsible about Insert, Delete, Update
   INSERT          { \p s -> TokenInsert p }
   INTO            { \p s -> TokenInto p }
   VALUES          { \p s -> TokenValues p }
   UPDATE          { \p s -> TokenUpdate p }
   SET             { \p s -> TokenSet p }
   DELETE          { \p s -> TokenDelete p }
    -- Joins 
   JOIN            { \p s -> TokenJoin p }
   INNER           { \p s -> TokenInner p }
   LEFT            { \p s -> TokenLeft p }
   RIGHT           { \p s -> TokenRight p }
   FULL            { \p s -> TokenFull p }
   OUTER           { \p s -> TokenOuter p }
   ON              { \p s -> TokenOn p }
    -- Logical Operators
   AND            { \p s -> TokenAnd p }
   OR             { \p s -> TokenOr p }
   NOT            { \p s -> TokenNot p }
   IN             { \p s -> TokenIn p }
   LIKE           { \p s -> TokenLike p }
    -- Conditional Expressions
   CASE           { \p s -> TokenCase p }
   WHEN           { \p s -> TokenWhen p }
   THEN           { \p s -> TokenThen p }
   ELSE           { \p s -> TokenElse p }
   END            { \p s -> TokenEnd p }
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
    --Parenthesis 
    "("           { \p s -> TokenLParen p }
    ")"           { \p s -> TokenRParen p }
    "["           { \p s -> TokenLBracket p }
    "]"           { \p s -> TokenRBracket p }
    "{"           { \p s -> TokenLBrace p }
    "}"           { \p s -> TokenRBrace p }
 -- ===== Pattern Matching =====
  $digit+(\.$digit+)? { \p s -> TokenNumber p (read s) }
  [$alpha _][$alpha _ $digit]*"."[$alpha _ $digit]+ { \p s -> TokenFilename p s }
  \"[$alpha $white \_ \- \, \. \? \[ \] \( \) \! \@ \$ \% \^ \& \* \+ \{ \} \` \~ $digit]*\" { \p s -> TokenString p (init (tail s)) }
  $ident { \p s -> TokenIdentifier p s }