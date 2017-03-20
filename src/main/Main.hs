import GolfASM

import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    finalState <- runProg (args !! 0)
    putStrLn $ showMS finalState
    return ()
