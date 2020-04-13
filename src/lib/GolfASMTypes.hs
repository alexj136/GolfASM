module GolfASMTypes where

import Data.Map (Map)

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

    | OpIntAdd
    | OpIntSub
    | OpIntMul
    | OpIntDiv
    | OpIntMod
    deriving (Show, Eq, Ord)

data Value
    = ValInt  Int
    | ValChar Char
    | ValList [Command]
    deriving (Show, Eq, Ord)

immediateValue :: Command -> Value
immediateValue (Immediate v) = v
immediateValue _ = error "not an Immediate"

valChar :: Value -> Char
valChar (ValChar c) = c
valChar _ = error "not a ValChar"

valList :: Value -> [Command]
valList (ValList c) = c
valList _ = error "not a ValList"

valuesAsString :: [Value] -> String
valuesAsString = map valChar

valueAsString :: Value -> String
valueAsString = map (valChar . immediateValue) . valList
