module Eval.MTL where

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

runMTL :: Expr -> Either Error (Value, [String])
runMTL e = runExcept $ runWriterT $ evalStateT (runReaderT (evalMTL e) initLexEnv) initEnv

evalMTL :: Expr -> EvalMTL Value
evalMTL (Constant n) = pure n
evalMTL (Var var) = do
    lexEnv <- ask
    case lookup var lexEnv of
        Just val -> pure val
        Nothing -> do
            globEnv <- gets fst
            case lookup var globEnv of
                Nothing -> throwError $ UnboundVariable var

{- | Define String Expr
    | Set String Expr
    | Op Operator Expr Expr
    | If Expr Expr Expr
    | Let [(String, Expr)] Expr
    | Begin [Expr]
    | Try Expr Expr
    | Fail Expr
    | Log Expr
    | History
    | ShowEnv
    | Reset
    | Quit
-}