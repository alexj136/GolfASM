module GolfASM where

import Data.Char (toLower, isSpace)
import qualified Data.Map as M
import System.Exit

import GolfASMTypes

-- Run a code buffer with an empty machine until the code buffer is empty
runProg :: Commands -> IO Machine
runProg commands = run (commands, [], M.empty)

-- Run a machine until the code buffer is empty
run :: Machine-> IO Machine
run machine@(commands, stack, registers) =
    if null commands then return machine
    else do machine' <- step machine; run machine'

-- Run with debug log
debugRun :: Machine-> IO Machine
debugRun m@([], s, r) = return m
debugRun m@(p , s, r) = do
    m' <- step m
    putStrLn $ show m'
    debugRun m'

-- Perform a single execution step for the given machine, returning the
-- resulting machine.
step :: Machine-> IO Machine
step (command:commands, stack, registers) = case command of
    PushVar   register -> return (commands, tail stack, M.insert register (head stack) registers)
    PopVar    register -> return (commands, (registers M.! register):stack, registers)
    Immediate value    -> return (commands, value:stack, registers)
    OpCall             -> return ((valList (head stack)) ++ commands, tail stack, registers)
    OpCond             -> return (commands, stack', registers) where
        stack' = case stack of
            ValInt 1 : t : f : rest -> t : rest
            ValInt 0 : t : f : rest -> f : rest
            _                       -> error "bad stack for conditionals"
    OpPrint            -> do
        putStrLn (valueAsString (head stack))
        return (commands, tail stack, registers)
    OpListConcat       -> return (commands, stack', registers) where
        stack' = case stack of
            ValList l1 : ValList l2 : rest -> ValList (l1 ++ l2) : rest
            _ -> error "bad stack for list concat"
    OpListHead         -> return (commands, stack', registers) where
        stack' = case stack of
            ValList l1 : rest -> immediateValue (head l1) : rest
            _ -> error "bad stack for list head"
    OpListTail         -> return (commands, stack', registers) where
        stack' = case stack of
            ValList l1 : rest -> ValList (tail l1) : rest
            _ -> error "bad stack for list tail"
    OpIntBinary op     -> return (commands, stack', registers) where
        stack' = case stack of
            ValInt i1 : ValInt i2 : rest -> ValInt ((toFn op) i1 i2) : rest
            _ -> error $ "bad stack for integer " ++ show op
