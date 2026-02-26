module Main where

import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))
import Parser
import Eval

main :: IO ()
main = hSetBuffering stdout NoBuffering >> repl

repl :: IO ()
repl = do
    putStr "MPL> "
    l <- getLine
    case parser l of
        Right expr -> let t = tc expr in putStrLn $ show t ++ " : " ++ show expr
        Left err -> print err
    repl