module Parser where

import Data.List (elemIndex)
import Data.Char (isUpper, isLower, toLower)

import Lexer
import GolfASMTypes

upper, lower :: String
upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
lower = "abcdefghijklmnopqrstuvwxyz"

-- TODO: rewrite so it can be tested properly
parse :: [Token] -> [[Command]] -> [Command]
parse [] stack = case length stack of
    1         -> head stack
    n | n > 1 -> error "parser: unclosed paren"
    n | n < 1 -> error "parser: extra closing paren"
parse (currentTk:rest) stack = case currentTk of
    Tk '[' -> parse rest ([] : stack)
    Tk ']' -> (Immediate . ValList) (head stack) : parse rest (tail stack)
    _      -> parse rest ((head stack ++ [easyParse currentTk]) : tail stack)

easyParse :: Token -> Command
easyParse token = case token of
    TkInt i -> Immediate (ValInt i)
    TkStr s -> Immediate (ValList (map (Immediate . ValChar) s))
    Tk c | isUpper c -> PushVar (toLower c)
    Tk c | isLower c -> PopVar  c
    Tk '.' -> OpCall
    Tk '?' -> OpCond
    Tk '$' -> OpPrint
    Tk ':' -> OpListConcat
    Tk '|' -> OpListHead
    Tk '#' -> OpListTail
    Tk '+' -> OpIntBinary Add
    Tk '-' -> OpIntBinary Sub
    Tk '*' -> OpIntBinary Mul
    Tk '/' -> OpIntBinary Div
    Tk '%' -> OpIntBinary Mod
    _ -> error "unrecognised token type in parser"
