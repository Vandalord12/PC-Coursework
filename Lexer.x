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
  -- A number
  $digit+(\.$digit+)? { \p s -> TokenNumber p (read s) }
  -- A file name
  [$alpha _][$alpha _ $digit]*"."[$alpha _ $digit]+ { \p s -> TokenFilename p s }
  -- A string
  \"[$alpha $white \_ \- \, \. \? \[ \] \( \) \! \@ \$ \% \^ \& \* \+ \{ \} \` \~ $digit]*\" { \p s -> TokenString p (init (tail s)) }
  -- An identifier
  $ident { \p s -> TokenIdentifier p s }

{
data Token =
  TokenSelect AlexPosn   |
  TokenFrom AlexPosn      |
  TokenWhere AlexPosn     |
  TokenOrder AlexPosn     |
  TokenBy AlexPosn        |
  TokenAsc AlexPosn       |
  TokenDesc AlexPosn      |
  TokenGroup AlexPosn     |
  TokenHaving AlexPosn    |
  TokenLimit AlexPosn     |
  TokenOffset AlexPosn    |
  TokenDistinct AlexPosn  |
    -- This chunck is responsible about Insert, Delete, Update
  TokenInsert AlexPosn   |
  TokenInto AlexPosn     |
  TokenValues AlexPosn   |
  TokenUpdate AlexPosn   |
  TokenSet AlexPosn      |
  TokenDelete AlexPosn   |
    -- Joins 
  TokenJoin AlexPosn     |
  TokenInner AlexPosn    |
  TokenLeft AlexPosn     |
  TokenRight AlexPosn    |
  TokenFull AlexPosn     |
  TokenOuter AlexPosn    |
  TokenOn AlexPosn       |
    -- Logical Operators
  TokenAnd AlexPosn      |
  TokenOr AlexPosn       |
  TokenNot AlexPosn      |
  TokenIn AlexPosn       |
  TokenLike AlexPosn 
  
     -- Conditional Expressions
  TokenCase AlexPosn     |
  TokenWhen AlexPosn     |
  TokenThen AlexPosn     |
  TokenElse AlexPosn     |
  TokenEnd AlexPosn      |
   -- Essential arthimitic Operators
  TokenPlus AlexPosn     |
  TokenMinus AlexPosn    |
  TokenMultiply AlexPosn |
  TokenDivide AlexPosn   |
  TokenModulo AlexPosn   |
  -- Comparison Operators
  TokenEquals AlexPosn   |
  TokenNotEquals AlexPosn|
  TokenLessThan AlexPosn |
  TokenGreaterThan AlexPosn |
  TokenLessThanEq AlexPosn |
  TokenGreaterThanEq AlexPosn |
  --Parenthesis 
  TokenLParen AlexPosn |
  TokenRParen AlexPosn |
  TokenLBracket AlexPosn |
  TokenRBracket AlexPosn |
  TokenLBrace AlexPosn |
  TokenRBrace  AlexPosn |
  TokenNumber AlexPosn Num |
  TokenFilename AlexPosn String |
  
  -- What are the types for these two ???
  TokenString AlexPosn String |
  TokenIdentifier AlexPosn String
  deriving(Eq,Show)

  tokenPosn :: Token -> String
  tokenPosn (TokenSelect (AlexPn _ l c)) = show l ++ ":" ++ show c
  tokenPosn (TokenFrom (AlexPn _ l c)) = show l ++ ":" ++ show c
  tokenPosn (TokenWhere (AlexPn _ l c)) = show l ++ ":" ++ show c
  tokenPosn (TokenOrder (AlexPn _ l c)) = show l ++ ":" ++ show c
  tokenPosn (TokenBy (AlexPn _ l c)) = show l ++ ":" ++ show c
  tokenPosn (TokenAsc (AlexPn _ l c)) = show l ++ ":" ++ show c
  tokenPosn (TokenDesc (AlexPn _ l c)) = show l ++ ":" ++ show c
  tokenPosn (TokenGroup (AlexPn _ l c)) = show l ++ ":" ++ show c
  tokenPosn (TokenHaving (AlexPn _ l c)) = show l ++ ":" ++ show c
  tokenPosn (TokenLimit (AlexPn _ l c)) = show l ++ ":" ++ show c
  tokenPosn (TokenOffset (AlexPn _ l c)) = show l ++ ":" ++ show c
  tokenPosn (TokenDistinct (AlexPn _ l c)) = show l ++ ":" ++ show c
  tokenPosn (TokenAll (AlexPn _ l c)) = show l ++ ":" ++ show c
  tokenPosn (TokenAny (AlexPn _ l c)) = show l ++ ":" ++ show c
  tokenPosn (TokenBetween (AlexPn _ l c)) = show l ++ ":" ++ show c
  tokenPosn (TokenUnion (AlexPn _ l c)) = show l ++ ":" ++ show c
  tokenPosn (TokenIntersect (AlexPn _ l c)) = show l ++ ":" ++ show c
  tokenPosn (TokenExcept (AlexPn _ l c)) = show l ++ ":" ++ show c

  -- Insert/Update/Delete
  tokenPosn (TokenInsert (AlexPn _ l c)) = show l ++ ":" ++ show c
  tokenPosn (TokenInto (AlexPn _ l c)) = show l ++ ":" ++ show c
  tokenPosn (TokenValues (AlexPn _ l c)) = show l ++ ":" ++ show c
  tokenPosn (TokenUpdate (AlexPn _ l c)) = show l ++ ":" ++ show c
  tokenPosn (TokenSet (AlexPn _ l c)) = show l ++ ":" ++ show c
  tokenPosn (TokenDelete (AlexPn _ l c)) = show l ++ ":" ++ show c

  -- Joins
  tokenPosn (TokenJoin (AlexPn _ l c)) = show l ++ ":" ++ show c
  tokenPosn (TokenInner (AlexPn _ l c)) = show l ++ ":" ++ show c
  tokenPosn (TokenLeft (AlexPn _ l c)) = show l ++ ":" ++ show c
  tokenPosn (TokenRight (AlexPn _ l c)) = show l ++ ":" ++ show c
  tokenPosn (TokenFull (AlexPn _ l c)) = show l ++ ":" ++ show c
  tokenPosn (TokenOuter (AlexPn _ l c)) = show l ++ ":" ++ show c
  tokenPosn (TokenOn (AlexPn _ l c)) = show l ++ ":" ++ show c

  -- Logical Operators
  tokenPosn (TokenAnd (AlexPn _ l c)) = show l ++ ":" ++ show c
  tokenPosn (TokenOr (AlexPn _ l c)) = show l ++ ":" ++ show c
  tokenPosn (TokenNot (AlexPn _ l c)) = show l ++ ":" ++ show c
  tokenPosn (TokenIn (AlexPn _ l c)) = show l ++ ":" ++ show c
  tokenPosn (TokenLike (AlexPn _ l c)) = show l ++ ":" ++ show c

  -- Conditional Expressions
  tokenPosn (TokenCase (AlexPn _ l c)) = show l ++ ":" ++ show c
  tokenPosn (TokenWhen (AlexPn _ l c)) = show l ++ ":" ++ show c
  tokenPosn (TokenThen (AlexPn _ l c)) = show l ++ ":" ++ show c
  tokenPosn (TokenElse (AlexPn _ l c)) = show l ++ ":" ++ show c
  tokenPosn (TokenEnd (AlexPn _ l c)) = show l ++ ":" ++ show c

  -- Arithmetic Operators
  tokenPosn (TokenPlus (AlexPn _ l c)) = show l ++ ":" ++ show c
  tokenPosn (TokenMinus (AlexPn _ l c)) = show l ++ ":" ++ show c
  tokenPosn (TokenMultiply (AlexPn _ l c)) = show l ++ ":" ++ show c
  tokenPosn (TokenDivide (AlexPn _ l c)) = show l ++ ":" ++ show c
  tokenPosn (TokenModulo (AlexPn _ l c)) = show l ++ ":" ++ show c

  -- Comparison Operators
  tokenPosn (TokenEquals (AlexPn _ l c)) = show l ++ ":" ++ show c
  tokenPosn (TokenNotEquals (AlexPn _ l c)) = show l ++ ":" ++ show c
  tokenPosn (TokenLessThan (AlexPn _ l c)) = show l ++ ":" ++ show c
  tokenPosn (TokenGreaterThan (AlexPn _ l c)) = show l ++ ":" ++ show c
  tokenPosn (TokenLessThanEq (AlexPn _ l c)) = show l ++ ":" ++ show c
  tokenPosn (TokenGreaterThanEq (AlexPn _ l c)) = show l ++ ":" ++ show c

  -- Parenthesis/Brackets/Braces
  tokenPosn (TokenLParen (AlexPn _ l c)) = show l ++ ":" ++ show c
  tokenPosn (TokenRParen (AlexPn _ l c)) = show l ++ ":" ++ show c
  tokenPosn (TokenLBracket (AlexPn _ l c)) = show l ++ ":" ++ show c
  tokenPosn (TokenRBracket (AlexPn _ l c)) = show l ++ ":" ++ show c
  tokenPosn (TokenLBrace (AlexPn _ l c)) = show l ++ ":" ++ show c
  tokenPosn (TokenRBrace (AlexPn _ l c)) = show l ++ ":" ++ show c

}

   
