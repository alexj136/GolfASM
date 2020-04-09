import GolfASM

import Lexer
import System.Environment (getArgs)
import qualified Data.Map as M

main :: IO ()
main = do
    args <- getArgs
    case args of
        "-e" : prog : args -> do
            finalState <- run
                (prog, map codeToExp args, M.empty, M.empty)
            return ()
        file : args -> do
            prog <- readFile file
            finalState <- run
                (prog, map codeToExp args, M.empty, M.empty)
            return ()
