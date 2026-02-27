{-# LANGUAGE TemplateHaskell #-}

module MPL where

import Syntax
import Control.Lens
import Control.Monad.Trans.State.Lazy (State)

data MplExpr = MplExpr
    { _expr :: Expr
    , _exprVal :: Value
    , _exprType :: Types
    }
makeLenses ''MplExpr

newtype M a = M { runM :: State MplExpr a }

builtIns :: [(Id, Expr -> Value)]
builtIns =
    [ ("transcribe", transcribe)
    , ("translate", translate)
    , ("kmer", kmer)
    , ("complement", complement)
    , ("reverse_complement", reverseComplement)
    ]

transcribe :: Expr -> Value
transcribe = undefined

translate :: Expr -> Value
translate = undefined

kmer :: Expr -> Value
kmer = undefined

complement :: Expr -> Value
complement = undefined

reverseComplement :: Expr -> Value
reverseComplement = undefined