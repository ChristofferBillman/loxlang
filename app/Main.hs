module Main (main) where
import Scanner (scanTokens)
import Parser (parse)
import Interpreter (interpret)
import System.Directory.Internal.Prelude (getArgs)
import Debug.Trace

main :: IO ()
main = do
    (arg:args) <- getArgs
    contents   <- traceShow (arg:args) readFile arg
    mapM_ print $ interpret $ parse $ scanTokens contents