module Language.Environment where

import Language.Expr

type Env = [(String, Value)]

empty :: Env
empty = []

extend :: (String, Value) -> Env -> Either Error Env
extend (var, val) env = 
    let mval = lookup var Env
    in case mval of
        Nothing -> Right (env ++ [(var,val)])
        Just val' -> Left 

set :: (String, Value) -> Env -> Either Error Env
set var val env = undefined