module LexerTest (allTests) where

import Lexer
import TestUtils

allTests :: [Test]
allTests =
    [ test "simple lexer" ("ab+12" `lexesAs` [TkVar "ab", Tk '+', TkInt 12])
    ]

lexesAs :: String -> [Token] -> Bool
lexesAs inputString outputTokens = map fst (scan inputString) == outputTokens
