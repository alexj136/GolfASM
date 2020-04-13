module Parser where

import Lexer
import GolfASMTypes

upper, lower :: String
upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
lower = "abcdefghijklmnopqrstuvwxyz"

parse :: [Token] -> [Command]
parse [] = []
parse (token:tokens) = case token of
    TkInt i -> Immediate (ValInt i)                              : parse tokens
    TkStr s -> Immediate (ValList (map (Immediate . ValChar) s)) : parse tokens
    Tk c | elem c upper -> PushVar c : parse tokens
    Tk c | elem c lower -> PopVar  c : parse tokens
    Tk '.' -> OpCall       : parse tokens
    Tk '?' -> OpCond       : parse tokens
    Tk '$' -> OpPrint      : parse tokens
    Tk ':' -> OpListConcat : parse tokens
    Tk '|' -> OpListHead   : parse tokens
    Tk '#' -> OpListTail   : parse tokens
    Tk '+' -> OpIntAdd     : parse tokens
    Tk '-' -> OpIntSub     : parse tokens
    Tk '*' -> OpIntMul     : parse tokens
    Tk '/' -> OpIntDiv     : parse tokens
    Tk '%' -> OpIntMod     : parse tokens
    Tk '[' -> undefined -- see brainhask
    Tk ']' -> undefined -- see brainhask
