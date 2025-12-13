module Language.Environment where

import Language.Expr

type Env = [(String, Value)]

empty :: Env
empty = []

extend :: (String, Value) -> Env -> Either Error Env
extend (var, val) env = 
    let mval = lookup var env
    in case mval of
        Nothing -> Right (env ++ [(var,val)])
        Just _ -> Left (MultipleDeclarations var)

set :: (String, Value) -> Env -> Either Error Env
set (var, val) env = 
    let mval = lookup var env
    in case mval of
        Nothing -> Left (UnboundVariable var)
        Just _ -> Right (upsert env (var, val))

upsert :: Env -> (String, Value) -> Env
upsert env (var, val) = [(x,y) | (x,y) <- env, x /= var] ++ [(var,val)]