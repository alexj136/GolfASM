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
    OpCond             -> return (commands, undefined, undefined)
    OpPrint            -> do
        putStrLn (valueAsString (head stack))
        return (commands, tail stack, registers)
    OpListConcat       -> return (commands, undefined, undefined)
    OpListHead         -> return (commands, undefined, undefined)
    OpListTail         -> return (commands, undefined, undefined)
    OpIntAdd           -> return (commands, undefined, undefined)
    OpIntSub           -> return (commands, undefined, undefined)
    OpIntMul           -> return (commands, undefined, undefined)
    OpIntDiv           -> return (commands, undefined, undefined)
    OpIntMod           -> return (commands, undefined, undefined)
