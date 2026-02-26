module Parser where
    
import Data.Functor.Identity (Identity)
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.String (Parser)

import Lexer
import Syntax

binary :: String -> Op -> Assoc -> Operator String () Identity Expr
binary s op = Infix (mplReservedOp s >> return (BinOp op))

prefix :: String -> Op -> Operator String () Identity Expr
prefix s op = Prefix (mplReservedOp s >> return (UnOp op))

prefixK :: String -> Op -> Operator String () Identity Expr
prefixK s op = Prefix (mplReserved s >> return (UnOp op))

opTable :: OperatorTable String () Identity Expr
opTable =
    [ [ prefix "-" Sub, prefixK "not" Not ]
    , [ binary "^"  Pow  AssocRight ]
    , [ binary "*"  Mul  AssocLeft, binary "/"  Div AssocLeft ]
    , [ binary "+"  Add  AssocLeft, binary "-"  Sub AssocLeft ]
    , [ binary "<"  Lt   AssocNone, binary ">"  Gt    AssocNone
      , binary "<=" LtEq AssocNone, binary ">=" GtEq  AssocNone ]
    , [ binary "==" Eq   AssocNone, binary "!=" NotEq AssocNone ]
    , [ binary "&&" And  AssocLeft ]
    , [ binary "||" Or   AssocLeft ]
    , [ binary "|>" Pipe AssocLeft ]
    ]

parseInt :: Parser Expr
parseInt = Const . IntV <$> mplNatural

parseBool :: Parser Expr
parseBool = Const . BoolV <$> (True <$ mplReserved "true" <|> False <$ mplReserved "false")

parseVar :: Parser Expr
parseVar = Var <$> mplIdentifier

parseAtom :: Parser Expr
parseAtom = mplParens parseExpr <|> parseInt <|> parseBool <|> parseVar

parseApp :: Parser Expr
parseApp = foldl1 App <$> many1 parseAtom

parseTerm :: Parser Expr
parseTerm = buildExpressionParser opTable parseApp

parseIf :: Parser Expr
parseIf = do
    mplReserved "if"
    cnd <- parseExpr
    mplReserved "then"
    e0 <- parseExpr
    mplReserved "else"
    If cnd e0 <$> parseExpr

parseLet :: Parser Expr
parseLet = do
    mplReserved "let"
    v <- mplIdentifier
    mplReservedOp "="
    e0 <- parseExpr
    mplReserved "in"
    Let v e0 <$> parseExpr

parseLetF :: Parser Expr
parseLetF = do
    mplReserved "let"
    f <- mplIdentifier
    args <- many mplIdentifier
    mplReservedOp "="
    LetF f args <$> parseExpr

parseLetR :: Parser Expr
parseLetR = do
    mplReserved "let"
    mplReserved "rec"
    f <- mplIdentifier
    args <- many mplIdentifier
    mplReservedOp "="
    LetR f args <$> parseExpr

parseLam :: Parser Expr
parseLam = do
    mplReserved "lambda"
    args <- many mplIdentifier
    mplReservedOp "->"
    Lam args <$> parseExpr

parseExpr :: Parser Expr
parseExpr = try parseLetR <|> try parseLet <|> try parseLetF <|> parseLam <|> parseIf <|> parseTerm

parser :: String -> Either ParseError Expr
parser = parse parseExpr "mpl"