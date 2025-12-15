module Eval.MTL (mtlREPL) where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Except
import Language

type EvalMTL a = ReaderT Env (StateT (Env, [String]) (WriterT [String] (Except Error))) a

initEnv :: (Env, [String])
initEnv = ([], [])

initLexEnv :: Env
initLexEnv = []

run :: (Env, [String]) -> Expr -> Either Error ((Value, (Env, [String])), [String])
run env e = runExcept $ runWriterT $ runStateT (runReaderT (evalMTL e) initLexEnv) env

evalMTL :: Expr -> EvalMTL Value
evalMTL (Constant n) = pure n
evalMTL (Var var) = do
    lexEnv <- ask
    case lookup var lexEnv of
        Just expr -> evalMTL expr
        Nothing -> do
            globEnv <- gets fst
            case lookup var globEnv of
                Nothing -> throwError $ UnboundVariable var
                Just expr -> evalMTL expr
evalMTL d@(Define var expr) = do
    (globalEnv, hist) <- get
    let menv = extend (var, expr) globalEnv
    case menv of
        Left err -> throwError err
        Right newGlobalEnv -> put (newGlobalEnv, hist ++ [show d]) >> pure Void
evalMTL s@(Set var expr) = do
    lexEnv <- ask
    case lookup var lexEnv of
        Nothing -> do
            (globalEnv, hist) <- get
            case lookup var globalEnv of
                Nothing -> throwError $ UnboundVariable var
                Just _ -> put (upsert globalEnv (var, expr), hist ++ [show s]) >> pure Void
        Just _ -> undefined
evalMTL (Op op e1 e2) = undefined
evalMTL (If cnd thn els) = undefined
evalMTL (Let bindings expr) = undefined
evalMTL (Begin exprs) = undefined
evalMTL (Try e1 e2) = undefined
evalMTL (Fail expr) = undefined
evalMTL (Log expr) = undefined
evalMTL History = pure Void
evalMTL ShowEnv = pure Void
evalMTL Reset = pure Void
evalMTL Quit = pure Void

mtlREPL :: IO ()
mtlREPL = go initEnv
    where
        go e = do
            putStr "MTL> "
            l <- getLine
            if null l 
                then go e 
                else case parse l of
                        Left err -> putStrLn ("Parse error: " ++ err) >> go e
                        Right expr -> 
                            case run e expr of
                                Left err -> print err >> go e
                                Right ((v, st), _) -> print v >> go st