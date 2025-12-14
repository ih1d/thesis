module Language.Environment where

import Language.Expr

type Env = [(String, Expr)]

empty :: Env
empty = []

extend :: (String, Expr) -> Env -> Either Error Env
extend (var, expr) env = 
    let mval = lookup var env
    in case mval of
        Nothing -> Right (env ++ [(var,expr)])
        Just _ -> Left (MultipleDeclarations var)

set :: (String, Expr) -> Env -> Either Error Env
set (var, expr) env = 
    let mval = lookup var env
    in case mval of
        Nothing -> Left (UnboundVariable var)
        Just _ -> Right (upsert env (var, expr))

upsert :: Env -> (String, Expr) -> Env
upsert env (var, expr) = [(x,y) | (x,y) <- env, x /= var] ++ [(var,expr)]