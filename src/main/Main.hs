import GolfASM

import Lexer
import Parser
import System.Environment (getArgs)
import qualified Data.Map as M

-- ./GolfASM -e "the program as string" "the program's input"
-- ./GolfASM programFilePath "the program's input"

main :: IO ()
main = do
    args <- getArgs
    case args of
        "-e" : prog : args -> do
            finalState <- run
                (parse (scanNoPos prog) [[]], [] ,M.empty)
            return ()
        file : args -> do
            prog <- readFile file
            finalState <- run
                (parse (scanNoPos prog) [[]], [], M.empty)
            return ()
