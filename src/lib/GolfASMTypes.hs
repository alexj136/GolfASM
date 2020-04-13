module GolfASMTypes where

import Data.Map (Map)
import Data.List (intersperse)

type Machine = (Commands, ValueStack, Registers)
type Commands = [Command]
type Registers = Map Char Value
type ValueStack = [Value]

-- The GolfASM machine has registers with single character names. They can be
-- pushed or popped to the stack. Lower-case pops, upper-case pushes.
type Register = Char

data Command
    = PushVar Register	
    | PopVar Register

    | Immediate Value -- immediates get pushed

    | OpCall -- assumes the top of the stack is a command list - pops it
             -- to the executing command list to run next
    
    | OpCond -- stack is: NonZeroInt A B rest -> A rest
             -- stack is: Zero       A B rest -> B rest
             -- stack is: NonIntThing rest    -> error

    | OpPrint -- print what's on top of the stack

    | OpListConcat
    | OpListHead
    | OpListTail

    | OpIntBinary IntBinaryOp
    deriving (Show, Eq, Ord)

data Value
    = ValInt  Int
    | ValChar Char
    | ValList [Command]
    deriving (Eq, Ord)

instance Show Value where
    show (ValInt  i) = show i
    show (ValChar c) = show c
    show (ValList l) = '[' : (concat $ intersperse ", " $ map show l) ++ "]"

immediateValue :: Command -> Value
immediateValue (Immediate v) = v
immediateValue _ = error "not an Immediate"

valChar :: Value -> Char
valChar (ValChar c) = c
valChar _ = error "not a ValChar"

valCharCommand :: Command -> Char
valCharCommand (Immediate (ValChar c)) = c
valCharCommand _                       = error "not a ValChar"

isValChar :: Value -> Bool
isValChar (ValChar c) = True
isValChar _           = False

isValCharCommand :: Command -> Bool
isValCharCommand (Immediate (ValChar c)) = True
isValCharCommand _                       = False

valList :: Value -> [Command]
valList (ValList c) = c
valList _ = error "not a ValList"

valuesAsString :: [Value] -> String
valuesAsString = map valChar

valueAsString :: Value -> String
valueAsString v = case v of 
    ValList l | all isValCharCommand l -> map valCharCommand l
    _ -> show v

data IntBinaryOp = Add | Sub | Mul | Div | Mod deriving (Eq, Ord)

instance Show IntBinaryOp where
    show Add = "addition"
    show Sub = "subtraction"
    show Mul = "multiplucation"
    show Div = "division"
    show Mod = "modulo"

toFn :: IntBinaryOp -> (Int -> Int -> Int)
toFn ibo = case ibo of
    Add -> (+)
    Sub -> (-)
    Mul -> (*)
    Div -> div
    Mod -> mod
