module Main where

import System.Environment (getArgs)
import System.IO (hSetBuffering, stdin, BufferMode(NoBuffering))

main :: IO ()
main = do
    args <- getArgs
    case args of
        [backend] -> repl backend
        _ -> putStrLn "Usage: mini-scheme [backend]\nBackends: mtl, fused-effects, freer-simple, polysemy, effectful"

repl :: String -> IO ()
repl backend = do
    hSetBuffering stdin NoBuffering
    case backend of
        "mtl" -> undefined
        "fused-effects" -> undefined
        "freer-simple" -> undefined
        "polysemy" -> undefined
        "effectful" -> undefined
        _ -> error "Usage: mini-scheme [backend]\nBackends: mtl, fused-effects, freer-simple, polysemy, effectful"