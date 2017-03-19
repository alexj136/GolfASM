module Main where

import GolfASM

import qualified Data.Map as M

import Test.QuickCheck

main :: IO ()
main = quickCheck simple

simple = runProg "10" == ("", [IntE 10], M.empty, M.empty)
