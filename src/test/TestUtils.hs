module TestUtils where

data Test = Test String (IO Bool)

test :: String -> Bool -> Test
test description testExpression = Test description (return testExpression)

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
