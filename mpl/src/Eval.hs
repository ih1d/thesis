module Eval where

import Syntax

tc :: Expr -> Types
tc (Const (IntV _)) = IntT
tc (Const (BoolV _)) = BoolT
tc (UnOp Not (Const (BoolV _))) = BoolT
tc (UnOp _ _) = error "tc: only unary operator is not"
tc (BinOp op e0 e1) =
    case op of
        Add -> if tc e0 == IntT && tc e1 == IntT then IntT else error "tc: + expects integers"
        Sub -> if tc e0 == IntT && tc e1 == IntT then IntT else error "tc: - expects integers"
        Mul -> if tc e0 == IntT && tc e1 == IntT then IntT else error "tc: * expects integers"
        Div -> if tc e0 == IntT && tc e1 == IntT then IntT else error "tc: / expects integers"
        Pow -> if tc e0 == IntT && tc e1 == IntT then IntT else error "tc: ^ expects integers"
        And -> if tc e0 == BoolT && tc e1 == BoolT then BoolT else error "tc: && expects booleans"
        Or -> if tc e0 == BoolT && tc e1 == BoolT then BoolT else error "tc: || expects booleans"
        Eq -> if tc e0 == tc e1 then tc e0 else error "tc: == expects equal types"
        NotEq -> if tc e0 == tc e1 then tc e0 else error "tc: != expects equal types"
        Gt -> if tc e0 == IntT && tc e1 == IntT then BoolT else error "tc: > expects integers"
        GtEq -> if tc e0 == IntT && tc e1 == IntT then BoolT else error "tc: >= expects integers"
        Lt -> if tc e0 == IntT && tc e1 == IntT then BoolT else error "tc: < expects integers"
        LtEq -> if tc e0 == IntT && tc e1 == IntT then BoolT else error "tc: <= expects booleans"
        op' -> error $ "tc: " ++ show op' ++ " not defined"
tc (If cnd e0 e1) = 
    case tc cnd of
        BoolT -> if tc e0 == tc e1 then tc e0 else error "if expressions expect same type on both branches"
        _ -> error "tc: if expression expects boolean"
