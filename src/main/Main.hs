import GolfASM

import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    let finalState = runProg (args !! 0)
    putStrLn $ showMS finalState
    return ()
