{-# LANGUAGE LambdaCase #-}
module Language.Parser where

import Language.Expr
import Language.Lexer

type ParseError = String

newtype Parser a = Parser { runParser :: [LocatedToken] -> Either ParseError (a, [LocatedToken]) }

instance Functor Parser where
    fmap f (Parser p) = 
        Parser $ \s -> 
            case p s of
                Left err -> Left err
                Right (a, st') -> Right (f a, st')

instance Applicative Parser where
    pure a = Parser $ \st -> Right (a, st)
    Parser f <*> Parser p = Parser $ \st ->
        case f st of
            Left err -> Left err
            Right (f', st') -> 
                case p st' of
                    Left err -> Left err
                    Right (a, st'') -> Right (f' a, st'')

instance Monad Parser where
    return = pure
    Parser p >>= f = Parser $ \st ->
        case p st of
            Left err -> Left err
            Right (a, st') -> runParser (f a) st'

parseError :: String -> Parser a
parseError msg = Parser $ \_ -> Left msg

peek :: Parser LocatedToken
peek = Parser $ \st -> 
    case st of
        [] -> Left "Unexpected end of input"
        (t:_) -> Right (t, st)

advance :: Parser LocatedToken
advance = Parser $ \case
    [] -> Left "Unexpected end of input"
    (t:ts) -> Right (t, ts)

isEOF :: Parser Bool
isEOF = Parser $ \st -> case st of
    [] -> Right (True, st)
    (LocatedToken _ TokEOF : _) -> Right (True, st)
    _ -> Right (False, st)

expect :: Token -> String -> Parser ()
expect expected msg = do
    tok <- advance
    if token tok == expected
        then pure ()
        else parseError $ msg ++ " at line " ++ show (posLine (tokPos tok)) ++ ", column " ++ show (posColumn (tokPos tok)) ++ ". Got: " ++ show (token tok)

parse :: String -> Either ParseError Expr
parse input = do
    tokens <- lexer input
    case runParser parseExpr tokens of
        Left err -> Left err
        Right (e, st) -> 
            case st of
                [] -> Right e
                [LocatedToken _ TokEOF] -> Right e
                (t:_) -> Left $ "Unexpected token after expression: " ++ show (token t)

parseExpr :: Parser Expr
parseExpr = do
    tok <- peek
    case token tok of
        TokInt n     -> advance >> pure (Constant (NumV n))
        TokBool b    -> advance >> pure (Constant (BoolV b))
        TokString s  -> advance >> pure (Constant (StringV s))
        TokSymbol s  -> advance >> pure (Var s)
        LParen    -> parseList
        RParen    -> parseError $ "Unexpected ')' at line " ++ show (posLine (tokPos tok))
        TokEOF       -> parseError "Unexpected end of input"

parseList :: Parser Expr
parseList = do
    expect LParen "Expected '('"
    tok <- peek
    case token tok of
        RParen -> parseError "Empty list not allowed"
        TokSymbol s -> parseForm s
        _ -> parseError $ "Expected symbol at start of list, got: " ++ show (token tok)


parseForm :: String -> Parser Expr
parseForm s = case s of
    "define"  -> parseDefine
    "set!"    -> parseSetBang
    "if"      -> parseIf
    "let"     -> parseLet
    "begin"   -> parseBegin
    "try"     -> parseTry
    "fail"    -> parseFail
    "log"     -> parseLog
    "history" -> advance >> expect RParen "Expected ')'" >> pure History
    "env"     -> advance >> expect RParen "Expected ')'" >> pure ShowEnv
    "reset"   -> advance >> expect RParen "Expected ')'" >> pure Reset
    "quit"    -> advance >> expect RParen "Expected ')'" >> pure Quit
    "+"       -> parseBinOp (Op Add)
    "-"       -> parseBinOp (Op Sub)
    "*"       -> parseBinOp (Op Mul)
    "/"       -> parseBinOp (Op Div)
    "="       -> parseBinOp (Op Eq)
    "<"       -> parseBinOp (Op Lt)
    ">"       -> parseBinOp (Op Gt)
    _         -> parseError $ "Unknown form: " ++ s

parseDefine :: Parser Expr
parseDefine = do
    _ <- advance
    nameTok <- advance
    name <- case token nameTok of
        TokSymbol n -> pure n
        _           -> parseError "Expected symbol after 'define'"
    expr <- parseExpr
    expect RParen "Expected ')' after define"
    pure (Define name expr)

parseSetBang :: Parser Expr
parseSetBang = do
    _ <- advance
    nameTok <- advance
    name <- case token nameTok of
        TokSymbol n -> pure n
        _           -> parseError "Expected symbol after 'set!'"
    expr <- parseExpr
    expect RParen "Expected ')' after set!"
    pure (Set name expr)

parseIf :: Parser Expr
parseIf = do
    _ <- advance
    cond <- parseExpr
    thenE <- parseExpr
    elseE <- parseExpr
    expect RParen "Expected ')' after if"
    pure (If cond thenE elseE)

parseLet :: Parser Expr
parseLet = do
    _ <- advance
    expect LParen "Expected '(' for let bindings"
    bindings <- parseBindings
    expect RParen "Expected ')' after let bindings"
    body <- parseExpr
    expect RParen "Expected ')' after let"
    pure (Let bindings body)

parseBindings :: Parser [(String, Expr)]
parseBindings = do
    tok <- peek
    case token tok of
        RParen -> pure []
        LParen -> do
            binding <- parseOneBinding
            rest <- parseBindings
            pure (binding : rest)
        _ -> parseError "Expected binding or ')'"

parseOneBinding :: Parser (String, Expr)
parseOneBinding = do
    expect LParen "Expected '(' for binding"
    nameTok <- advance
    name <- case token nameTok of
        TokSymbol n -> pure n
        _           -> parseError "Expected symbol in binding"
    expr <- parseExpr
    expect RParen "Expected ')' after binding"
    pure (name, expr)

parseBegin :: Parser Expr
parseBegin = do
    _ <- advance
    exprs <- parseMany
    expect RParen "Expected ')' after begin"
    case exprs of
        [] -> parseError "begin requires at least one expression"
        _  -> pure (Begin exprs)

parseMany :: Parser [Expr]
parseMany = do
    tok <- peek
    case token tok of
        RParen -> pure []
        _         -> do
            e <- parseExpr
            rest <- parseMany
            pure (e : rest)

parseTry :: Parser Expr
parseTry = do
    _ <- advance
    expr <- parseExpr
    fallback <- parseExpr
    expect RParen "Expected ')' after try"
    pure (Try expr fallback)

parseFail :: Parser Expr
parseFail = do
    _ <- advance
    expr <- parseExpr
    expect RParen "Expected ')' after fail"
    pure (Fail expr)

parseLog :: Parser Expr
parseLog = do
    _ <- advance 
    expr <- parseExpr
    expect RParen "Expected ')' after log"
    pure (Log expr)

parseBinOp :: (Expr -> Expr -> Expr) -> Parser Expr
parseBinOp constructor = do
    _ <- advance  
    e1 <- parseExpr
    e2 <- parseExpr
    expect RParen "Expected ')' after operator"
    pure (constructor e1 e2)
