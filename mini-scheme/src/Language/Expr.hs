module Language.Expr where

data Value
    = NumV Int
    | BoolV Bool
    | StringV String
    | Void

data Type
    = NumT
    | BoolT
    | StringT

instance Show Type where
    show NumT = "number"
    show BoolT = "boolean"
    show StringT = "string"

instance Show Value where
    show (NumV n) = show n
    show (BoolV b) = if b then "#t" else "#f"
    show (StringV str) = str
    show Void = "#unspecific!"

data Expr
    = Constant Value
    | Var String
    | Define String Expr
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

data Operator
    = Add
    | Sub
    | Mul
    | Div
    | Eq
    | Lt
    | Gt

data Error
    = UnboundVariable String
    | MultipleDeclarations String
    | TypeMismatch String Type
    | DivisionByZero
    | ParserError String

instance Show Error where
    show (UnboundVariable x) = "Unbound variable: " ++ x
    show (MultipleDeclarations x) = "Multiple declarations of: " ++ x
    show DivisionByZero = "Infinity!"
    show (TypeMismatch expected got) = "Type mismatch, expected: " ++ expected ++ ", got: " ++ show got
    show (ParserError msg) = "Error while parsing: " ++ msg