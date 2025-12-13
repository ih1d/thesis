module Language.Lexer where

import Data.Char (isDigit, isAlpha, isSpace, isAlphaNum)

data Token
    = LParen
    | RParen
    | TokInt Int
    | TokBool Bool
    | TokString String
    | TokSymbol String
    | TokEOF
    deriving (Eq)

instance Show Token where
    show LParen = "("
    show RParen = ")"
    show (TokInt n) = show n
    show (TokBool b) = if b then "#t" else "#f"
    show (TokString str) = str
    show (TokSymbol sym) = sym
    show TokEOF = "end of file"

data Pos = Pos
    { posLine :: Int
    , posColumn :: Int
    } deriving (Show, Eq)

initPos :: Pos
initPos = Pos 1 1

advanceChar :: Char -> Pos -> Pos
advanceChar '\n' (Pos l _) = Pos (l + 1) 1
advanceChar _  (Pos l c) = Pos l (c + 1)

advanceString :: String -> Pos -> Pos
advanceString str p = foldr advanceChar p str

data LocatedToken = LocatedToken
    { tokPos :: Pos
    , token :: Token
    } deriving (Show, Eq)

data LexerState = LexerState
    { lexInput :: String
    , lexPos :: Pos
    } deriving (Show, Eq)

initLexer :: String -> LexerState
initLexer input = LexerState input initPos

lexer :: String -> Either String [LocatedToken]
lexer input = go (initLexer input) []
    where
        go :: LexerState -> [LocatedToken] -> Either String [LocatedToken]
        go st acc =
            case lexOne st of
                Left err -> Left err
                Right (locTok, st')
                    | token locTok == TokEOF -> Right (reverse (locTok : acc))
                    | otherwise -> go st' (locTok : acc)

lexOne :: LexerState -> Either String (LocatedToken, LexerState)
lexOne st@(LexerState input pos) =
    case input of
        [] -> Right (LocatedToken pos TokEOF, st)
        (c:cs) | isSpace c -> lexOne (LexerState cs (advanceChar c pos))
        (';':cs) ->
            let (_, rest) = break (== '\n') cs
                newPos = advanceString (';' : takeWhile (/= '\n') cs) pos
            in case rest of
                [] -> Right (LocatedToken newPos TokEOF, LexerState [] newPos)
                (_:rst) -> lexOne (LexerState rst (advanceChar '\n' newPos))
        ('(':cs) -> Right (LocatedToken pos LParen, LexerState cs (advanceChar '(' pos))
        (')':cs) -> Right (LocatedToken pos RParen, LexerState cs (advanceChar ')' pos))
        ('#':'t':cs)
            | not (continuesSymbol cs) -> Right (LocatedToken pos (TokBool True), LexerState cs (advanceString "#t" pos))
            | otherwise -> Left $ "Unexpected character '" ++ [head cs] ++ "' at line " ++ show (posLine pos) ++ ", column " ++ show (posColumn pos)
        ('#':'f':cs)
            | not (continuesSymbol cs) -> Right (LocatedToken pos (TokBool False), LexerState cs (advanceString "#f" pos))
            | otherwise -> Left $ "Unexpected character '" ++ [head cs] ++ "' at line " ++ show (posLine pos) ++ ", column " ++ show (posColumn pos)
        ('"': cs) -> lexString pos cs
        (c : cs)
            | isDigit c -> 
                let (digits, rest) = span isDigit (c:cs)
                in Right (LocatedToken pos (TokInt (read digits)), LexerState rest (advanceString digits pos))
        (c : cs)
            | isSymbolStart c ->
                let (sym, rest) = span isSymbolChar (c:cs)
                in Right (LocatedToken pos (TokSymbol sym), LexerState rest (advanceString sym pos))
        (c:_) -> Left $ "Unexpected character '" ++ [c] ++ "' at line " ++ show (posLine pos) ++ ", column " ++ show (posColumn pos)

lexString :: Pos -> String -> Either String (LocatedToken, LexerState)
lexString startPos = go (advanceChar '"' startPos) []
    where
        go :: Pos -> String -> String -> Either String (LocatedToken, LexerState)
        go _ _ [] = Left $ "Unterminated string starting at line " ++ show (posLine startPos) ++ ", column " ++ show (posColumn startPos)
        go pos acc ('"':cs) = Right (LocatedToken startPos (TokString (reverse acc)), LexerState cs (advanceChar '"' pos))
        go pos acc ('\\':c:cs) = 
            let escaped = 
                    case c of
                        'n'  -> '\n'
                        't'  -> '\t'
                        'r'  -> '\r'
                        '\\' -> '\\'
                        '"'  -> '"'
                        _    -> c
            in go (advanceString ['\\', c] pos) (escaped : acc) cs
        go pos acc (c:cs) = go (advanceChar c pos) (c : acc) cs

continuesSymbol :: String -> Bool
continuesSymbol [] = False
continuesSymbol (c: _) = isSymbolChar c

isSymbolStart :: Char -> Bool
isSymbolStart c = isAlpha c || c `elem` "!$%&*+-./:<=>?@^_~"

isSymbolChar :: Char -> Bool
isSymbolChar c = isAlphaNum c || c `elem` "!$%&*+-./:<=>?@^_~"