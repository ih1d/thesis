module Language.Lexer where

import Data.Char (isDigit, isSpace)

data Token
    = LParen
    | RParen
    | TokInt Int
    | TokBool Bool
    | TokString String
    | TokSymbol String
    | TokEOF
    deriving (Eq)

data Pos = Pos
    { posLine :: Int
    , posColumn :: Int
    }

startPos :: Pos
startPos = Pos 1 1

advanceChar :: Char -> Pos -> Pos
advanceChar '\n' (Pos l _) = Pos (l + 1) 1
advanceChar _  (Pos l c) = Pos l (c + 1)

advanceString :: String -> Pos -> Pos
advanceString str p = foldr advanceChar p str

data LocatedToken = LocatedToken
    { tokPos :: Pos
    , token :: Token
    }

data LexerState = LexerState
    { lexInput :: String
    , lexPos :: Pos
    }

initLexer :: String -> LexerState
initLexer input = LexerState input startPos

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
            let (c:rst) = dropWhile (/= '\n') cs
                newPos = advanceChar c pos
            in if null rst 
                then Right (LocatedToken newPos TokEOF, LexerState [] newPos)
                else lexOne (LexerState rst newPos)
        ('(':cs) -> Right (LocatedToken pos LParen, LexerState cs (advanceChar '(' pos))
        (')':cs) -> Right (LocatedToken pos RParen, LexerState cs (advanceChar ')' pos))
