{
module Lexer where
}

%wrapper "posn"

$digit = 0-9
$lower = a-z
$ntquo = ~[\"]

tokens :-
--    $lower+       { \p s -> (TkVar s              , p) }
    $digit+       { \p s -> (TkInt (read s)       , p) }
    \"[$ntquo]*\" { \p s -> (TkStr (init (tail s)), p) }
    .             { \p s -> (Tk (head s)          , p) }
{
data Token
    = Tk Char
--    | TkVar String
    | TkInt Int
    | TkStr String
    deriving (Show, Eq, Ord)

-- The lexer function
scan :: String -> [(Token, AlexPosn)]
scan = alexScanTokens
}
