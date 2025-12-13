module Language.Expr where

data Value
    = NumV Int
    | BoolV Bool
    | StringV String
    | Void

instance Show Value where
    show (NumV n) = show n
    show (BoolV b) = if b then "#t" else "#f"
    show (StringV str) = str
    show Void = "#unspecific!"
    
data Type
    = NumT
    | BoolT
    | StringT

instance Show Type where
    show NumT = "number"
    show BoolT = "boolean"
    show StringT = "string"

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

instance Show Expr where
    show (Constant v) = show v
    show (Var var) = var
    show (Define var expr) = "(define " ++ var ++ " " ++ show expr ++ ")"
    show (Set var expr) = "(set! " ++ var ++ " " ++ show expr ++ ")"
    show (Op op e1 e2) = "(" ++ show op ++ " " ++ show e1 ++ " " ++ show e2 ++ ")"
    show (If cnd thn els) = "(if " ++ show cnd ++ " " ++ show thn ++ " " ++ show els ++ ")"
    show (Let vars body) = "(let (" ++ showVars vars ++ ") " ++ show body ++ ")"
        where
            showVars :: [(String, Expr)] -> String
            showVars [] = ""
            showVars ((v,e):bndgs) = "(" ++ v ++ " " ++ show e ++ ") " ++ showVars bndgs
    show (Begin exprs) = "(begin " ++ concatMap show exprs ++ ")"
    show (Try e1 e2) = "(try " ++ show e1 ++ " " ++ show e2 ++ ")"
    show (Fail e) = "(fail " ++ show e ++ ")"
    show (Log e) = "(log " ++ show e ++ ")"
    show History = "(history)"
    show ShowEnv = "(env)"
    show Reset = "(reset)"
    show Quit = "(quit)"

data Operator
    = Add
    | Sub
    | Mul
    | Div
    | Eq
    | Lt
    | Gt
instance Show Operator where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"
    show Eq = "="
    show Lt = "<"
    show Gt = ">"

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