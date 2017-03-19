module GolfASM where

import Data.Char (toLower)
import qualified Data.Map as M

type MachineState = (String, [Exp], Heap, Registers)
type MemoryIndexedBy a = M.Map a Exp
type Heap = MemoryIndexedBy Integer
type Registers = MemoryIndexedBy Char

data Exp
    = ListE [Exp]
    | IntE  Integer
    | CharE Char
    deriving (Eq, Ord)

instance Show Exp where
    show e = case e of
        IntE  x -> show x
        CharE c -> show [c]
        ListE l | all (\g -> case g of CharE _ -> True; _ -> False) l ->
            show $ expToCode e
                | otherwise -> show l

showMS :: MachineState -> String
showMS (p, s, h, r) =
    (if p == "" then "" else "CODE  " ++ p ++ "\n") ++
    "STACK " ++ show s ++ "\n" ++
    "HEAP  " ++ show h ++ "\n" ++
    "REGS  " ++ show r ++ "\n"

-- Lookup a given register or heap value. Uninitialised memory defaults to 0.
get :: Ord a => a -> MemoryIndexedBy a -> Exp
get = M.findWithDefault (IntE 0)

-- Assign a register or heap value
put :: Ord a => a -> Exp -> MemoryIndexedBy a -> MemoryIndexedBy a
put = M.insert

-- Run a code buffer with an empty machine until the code buffer is empty
runProg :: String -> MachineState
runProg s = run (s, [], M.empty, M.empty)

-- Run a machine until the code buffer is empty
run :: MachineState -> MachineState
run m@("", s, h, r) = m
run m@(p , s, h, r) = run $ step m

-- Convert an expression into a haskell string (or executable code)
expToCode :: Exp -> String
expToCode (ListE es) = map (\e -> case e of CharE c -> c) es

-- Convert a haskel string (or executable code) into an expression
codeToExp :: String -> Exp
codeToExp = ListE . map CharE

-- Perform a single execution step for the given machine, returning the
-- resulting machine.
step :: MachineState -> MachineState
step (p, s, h, r) = case p of

    -- Nop (do nothing)
    ' ':q -> (q, s, h, r)

    -- Push an immediate decimal number
    d:q | d `elem` ['0'..'9'] -> (dropWhile (`elem` ['0'..'9']) p,
        IntE (read (takeWhile (`elem` ['0'..'9']) p)):s, h, r)

    -- Push code until matching '}'
    '{':q -> let (toCloseCurly, _) = matchSplit Curly [] q in
        (q, codeToExp ('{':toCloseCurly):s, h, r)

    -- Pop to program code if accumulator is non-zero, otherwise pop nowhere
    '}':q | get 'a' r == IntE 0 -> (q, tail s, h, r)
          | otherwise   -> (expToCode (head s) ++ p, tail s, h, r)

    -- Push code until matching ')', skip to just after the matching ')'
    '(':q -> let (toCloseRound, q') = matchSplit Round [] q in
        (tail q', codeToExp toCloseRound:s, h, r)

    -- Pop stack into heap address in accumulator
    '^':q -> (q, tail s, put (case get 'a' r of IntE x -> x) (head s) h, r)

    -- Push value at heap address in accumulator
    'v':q -> (q, (get (case get 'a' r of IntE x -> x) h):s, h, r)

    -- Pop into register
    a:q | a `elem` ['A'..'Z'] -> (q, tail s, h, put (toLower a) (head s) r)

    -- Push register
    a:q | a `elem` ['a'..'z'] -> (q, get a r:s, h, r)

    -- Add the integers on top of the stack
    '+':q -> (q, case s of IntE x:IntE y:z -> IntE (x+y):z, h, r)

    -- Subtract the second-to-top stack elem from the top stack elem
    '-':q -> (q, case s of IntE x:IntE y:z -> IntE (x-y):z, h, r)

    -- Increment the integer on top of the stack
    '↑':q -> (q, case s of IntE x:y -> IntE (x+1):y, h, r)

    -- Decrement the integer on top of the stack
    '↓':q -> (q, case s of IntE x:y -> IntE (x-1):y, h, r)

    -- Logical not the top of the stack
    '!':q -> (q, case s of IntE 0:z -> IntE 1:z; IntE n:z -> IntE 0:z, h, r)

    -- Discard the top of the stack
    '\\':q -> (q, tail s, h, r)

    -- Catchall error
    c:q -> error $ "Unrecognised character '" ++ c : "'"

-- Given an open parenthesis type, a stack of parentheses types (initially
-- empty) and a string, split the given string at the closing parenthesis, such
-- that the closing parenthesis begins the right of the split.
matchSplit :: ParenType -> [ParenType] -> String -> (String, String)
matchSplit pt [] (c:s) | closes pt c = ("",c:s)
matchSplit pt st (c:s)               = case parenInfo c of
    Just (True , pt') -> let (m, r) = matchSplit pt (pt':st) s in (c:m, r)
    Just (False, pt') -> case st of
        pt'':st' | pt' == pt'' -> let (m, r) = matchSplit pt st' s in (c:m, r)
                 | otherwise   -> error "Mismatched parentheses"
    Nothing  -> let (m, r) = matchSplit pt st s in (c:m, r)
matchSplit _  _  _                   = error "Mismatched parentheses"

-- True == Open, False == Close
parenInfo :: Char -> Maybe (Bool, ParenType)
parenInfo c = case c of
    '(' -> Just (True , Round )
    '{' -> Just (True , Curly )
    '[' -> Just (True , Square)
    ')' -> Just (False, Round )
    '}' -> Just (False, Curly )
    ']' -> Just (False, Square)
    _   -> Nothing

closes :: ParenType -> Char -> Bool
closes Round  ')' = True
closes Curly  '}' = True
closes Square ']' = True
closes _      _   = False

data ParenType = Round | Curly | Square deriving (Show, Eq, Ord)
