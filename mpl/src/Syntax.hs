module Syntax where

type Id = String

data Types
    = IntT
    | BoolT
    deriving (Eq)
instance Show Types where
    show IntT = "int"
    show BoolT = "bool"

data Value
    = IntV Integer
    | BoolV Bool
instance Show Value where
    show (IntV i) = show i
    show (BoolV True) = "true"
    show (BoolV False) = "false"

data Op
    = Add
    | Sub
    | Mul
    | Div
    | Pow
    | And
    | Or
    | Not
    | Eq
    | NotEq
    | Gt
    | GtEq
    | Lt
    | LtEq
instance Show Op where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"
    show Pow = "^"
    show And = "&&"
    show Or = "||"
    show Not = "not"
    show Eq = "=="
    show NotEq = "!="
    show Gt = ">"
    show GtEq = ">="
    show Lt = "<"
    show LtEq = "<="

data Expr
    = Const Value
    | UnOp Op Expr
    | BinOp Op Expr Expr
    | If Expr Expr Expr
instance Show Expr where
    show (Const v) = show v
    show (UnOp o e) = show o ++ " " ++ show e
    show (BinOp op e0 e1) = show e0 ++ " " ++ show op ++ " " ++ show e1
    show (If cnd e0 e1) = "if " ++ show cnd ++ " then " ++ show e0 ++ " else " ++ show e1
