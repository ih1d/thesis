module Eval.MTL where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Except
import Language

type EvalMTL a = ReaderT Env (StateT (Env, [String]) (WriterT [String] (Except Error))) a