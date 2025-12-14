module Main where

import System.Environment (getArgs)
import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))
import Eval
import Language

main :: IO ()
main = do
    args <- getArgs
    case args of
        [backend] -> run backend
        _ -> putStrLn "Usage: mini-scheme [backend]\nBackends: mtl, fused-effects, freer-simple, polysemy, effectful"

run :: String -> IO ()
run backend = do
    hSetBuffering stdout NoBuffering
    putStrLn $ "Mini-Scheme REPL (backend: " ++ backend ++ ")"
    putStrLn "Type (quit) to exit, (help) for commands\n"
    case backend of
        "mtl" -> mtlREPL
        _ -> undefined