module Main where

import TestUtils
import GolfASM
import qualified LexerTest as LT

import System.Exit
import qualified Data.Map as M

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
    LT.allTests ++
    [ makeFinalStateTest "Simple 1" "10" 
        ("", [IntE 10], M.empty, M.empty)
    , makeFinalStateTest "Simple 2" "10A"
        ("", [], M.empty, M.singleton 'a' (IntE 10))
    , makeFinalStackTopTest "fibonacci" "10A0B1C{cbc+CBa↓A}c" (IntE 89)
    ]
