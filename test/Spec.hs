import Interpreter (interpret) 
import Parser (parse)
import Scanner (scanTokens)

main :: IO ()
main = do
    expect        ["hello"] (i "print \"hello\";")      "SimplePrint"
    expectFromSrc ["0.0","1.0","2.0","3.0","4.0","5.0"] "WhileTest"
    expectFromSrc ["0.0","1.0","2.0","3.0","4.0","5.0"] "ForTest"
    expectFromSrc ["a", "d"]                            "IfTest"
    expectFromSrc ["global a", "inner a", "modified a"] "EnvironmentTest"
    expectFromSrc ["150.0", "false"]                    "ExprTest"

expect :: [String] -> [String] -> String -> IO ()
expect a b testName
    | a == b    = putStrLn $ "OK:   " ++ testName
    | otherwise = putStrLn $ "FAIL: " ++ testName ++ "\nExpected\n    " ++ show a ++ "\nbut got\n    " ++ show b 

expectFromSrc :: [String] -> String -> IO ()
expectFromSrc expectedOut testName = do
    contents <- readFile $ "test/" ++ testName ++ ".lox"
    expect expectedOut (i contents) testName

i :: String -> [String]
i snippet = interpret $ parse $ scanTokens snippet