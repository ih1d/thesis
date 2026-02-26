module Main where
    
import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))
import Parser

main :: IO ()
main = hSetBuffering stdout NoBuffering >> repl

repl :: IO ()
repl = do
    putStr "MPL> "
    l <- getLine
    print $ parser l
    repl