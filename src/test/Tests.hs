module Main where

import GolfASM

import System.Exit
import qualified Data.Map as M

data Test = Test String (IO Bool)

runTest :: Test -> IO Bool
runTest (Test s run) = do
    result <- run
    if not result then putStrLn $ "Test failed: " ++ s else return ()
    return result

runTests :: [Test] -> IO Bool
runTests ts = do
    putStrLn $ "\nRunning " ++ show (length ts) ++ " tests..."
    allResults <- sequence $ map runTest ts
    putStrLn $ show (length (filter id allResults)) ++ "/" ++
        show (length ts) ++ " tests passed."
    return $ and allResults

makeFinalStateTest :: String -> String -> MachineState -> Test
makeFinalStateTest desc prog expectRes = Test desc $ do
    res <- runProg prog
    return (res == expectRes)

makeFinalStackTopTest :: String -> String -> Exp -> Test
makeFinalStackTopTest desc prog expectRes = Test desc $ do
    (_, res:_, _, _) <- runProg prog
    return (res == expectRes)

main :: IO ExitCode
main = do
    passed <- runTests allTests
    exitWith $ if passed then ExitSuccess else ExitFailure 1

allTests :: [Test]
allTests =
    [ makeFinalStateTest "Simple 1" "10" 
        ("", [IntE 10], M.empty, M.empty)
    , makeFinalStateTest "Simple 2" "10A"
        ("", [], M.empty, M.singleton 'a' (IntE 10))
    , makeFinalStackTopTest "fibonacci" "10A0B1C{cbc+CBa↓A}c" (IntE 89)
    ]
