module Lexer where

import Data.Char (isLetter, isNumber)

data NumberType = Float | Integer deriving (Show, Eq)

data TokenType = KeyWord | Symbol | Identifier | EOF | Number NumberType deriving (Show, Eq)

type Row = Int

type Column = Int

data RowColumn = RowColumn Row Column deriving (Show)

data Token = Token
  { value :: String,
    tag :: TokenType,
    rowColumn :: RowColumn
  }
  deriving (Show)

data LexerResult = Success Token | Error String RowColumn | Skip String RowColumn deriving (Show)

keyWords =
  [ "program",
    "ident",
    "begin",
    "real",
    "if",
    "then",
    "end",
    "else",
    "integer",
    "while"
  ]

stopWords = [" ", "\n", "\t"]

symbols =
  [ ";",
    "*",
    "+",
    "-",
    "/",
    ":",
    "<",
    ">",
    ":=",
    "=",
    "<>",
    ">=",
    ",",
    ".",
    "(",
    ")",
    "$"
  ]

defineTokenType :: [Char] -> TokenType
defineTokenType token
  | token `elem` keyWords = KeyWord
  | token `elem` symbols = Symbol
  | otherwise = Identifier

isSymbol :: Char -> Bool
isSymbol x = [x] `elem` symbols

isStopWord :: Char -> Bool
isStopWord x = [x] `elem` stopWords

isStopWordOrSymbol :: Char -> Bool
isStopWordOrSymbol x = isStopWord x || isSymbol x

plusColumn :: RowColumn -> RowColumn
plusColumn (RowColumn x y) = RowColumn x (y + 1)

plusTab :: RowColumn -> RowColumn
plusTab (RowColumn x y) = RowColumn x (y + 4)

nextLine :: RowColumn -> RowColumn
nextLine (RowColumn x y) = RowColumn (x + 1) 1

numberLexer :: String -> String -> RowColumn -> LexerResult
numberLexer token [] rowColumn = Success $ Token token (Number Integer) rowColumn
numberLexer token (x : xs) rowColumn
  | isNumber x = numberLexer (token ++ [x]) xs (plusColumn rowColumn)
  | x == '.' = numberDotLexer (token ++ [x]) xs (plusColumn rowColumn)
  | isStopWordOrSymbol x = Success $ Token token (Number Integer) rowColumn
  | isLetter x = Success $ Token token (Number Integer) rowColumn
  | otherwise = Error ("Erro sintático:" ++ [x]) rowColumn

numberDotLexer :: String -> String -> RowColumn -> LexerResult
numberDotLexer token [] rowColumn = Error "Esperado um número" rowColumn
numberDotLexer token (x : xs) rowColumn
  | isNumber x = numberAfterDotLexer (token ++ [x]) xs $ plusColumn rowColumn
  | otherwise = Error ("Esperado um número, encontrado:" ++ [x]) rowColumn

numberAfterDotLexer :: String -> String -> RowColumn -> LexerResult
numberAfterDotLexer token [] rowColumn = Success $ Token token (Number Float) rowColumn
numberAfterDotLexer token (x : xs) rowColumn
  | isNumber x = numberAfterDotLexer (token ++ [x]) xs $ plusColumn rowColumn
  | isStopWordOrSymbol x = Success $ Token token (Number Float) rowColumn
  | otherwise = Error ("Esperado um número, encontrado:" ++ [x]) rowColumn

wordLexer :: String -> String -> RowColumn -> LexerResult
wordLexer token [] rowColumn = Success $ Token token (defineTokenType token) rowColumn
wordLexer token (x : xs) rowColumn
  | isLetter x = wordLexer (token ++ [x]) xs $ plusColumn rowColumn
  | isStopWordOrSymbol x = Success $ Token token (defineTokenType token) rowColumn
  | otherwise = Success $ Token token Identifier rowColumn

initComment :: String -> String -> RowColumn -> LexerResult
initComment token [] rowColumn = Error "Esperado final do comentário" rowColumn
initComment token (x : xs) rowColumn
  | x == '*' && y == '/' = Skip (token ++ "*/") rowColumn
  | x == '\n' = initComment (token ++ [x]) xs $ nextLine rowColumn
  | x == '\t' = initComment (token ++ [x]) xs $ plusTab rowColumn
  | otherwise = initComment (token ++ [x]) xs $ plusColumn rowColumn
  where
    (y : _) = xs

initComment' :: String -> String -> RowColumn -> LexerResult
initComment' token [] rowColumn = Error "Esperado final do comentário" rowColumn
initComment' token (x : xs) rowColumn
  | x == '}' = Skip (token ++ "}") $ plusColumn rowColumn
  | x == '\n' = initComment' (token ++ [x]) xs $ nextLine rowColumn
  | x == '\t' = initComment' (token ++ [x]) xs $ plusTab rowColumn
  | otherwise = initComment' (token ++ [x]) xs $ plusColumn rowColumn

symbolLexer :: String -> String -> RowColumn -> LexerResult
symbolLexer token [] rowColumn = Error "Erro sintático" rowColumn
symbolLexer token (x : xs) rowColumn
  | x == '<' && second == '>' = Success $ Token "<>" Symbol rowColumn
  | x == '>' && second == '=' = Success $ Token ">=" Symbol rowColumn
  | x == '<' && second == '=' = Success $ Token "<=" Symbol rowColumn
  | x == ':' && second == '=' = Success $ Token ":=" Symbol rowColumn
  | isSymbol x = Success $ Token [x] Symbol rowColumn
  | otherwise = Error ("Erro sintático: " ++ [x]) rowColumn
  where
    (second : _) = xs

isBeginComment :: String -> Bool
isBeginComment (x : y : ys) = x == '/' && y == '*'
isBeginComment _ = False

initialState :: String -> RowColumn -> LexerResult
initialState [] rowColumn = Success $ Token "" EOF rowColumn
initialState list rowColumn
  | isBeginComment list = initComment "/*" (tail xs) $ plusColumn $ plusColumn rowColumn
  | x == '{' = initComment' "{" xs $ plusColumn rowColumn
  | isLetter x = wordLexer "" list rowColumn
  | isNumber x = numberLexer "" list rowColumn
  | isSymbol x = symbolLexer "" list rowColumn
  | x == ' ' = initialState xs $ plusColumn rowColumn
  | x == '\n' = initialState xs $ nextLine rowColumn
  | x == '\t' = initialState xs $ plusTab rowColumn
  | otherwise = Error ("Erro lexico em:" ++ list) rowColumn
  where
    (x : xs) = list

countFirstUselessWords :: Num p => String -> p
countFirstUselessWords [] = 0
countFirstUselessWords (x : xs)
  | x == ' ' || x == '\n' || x == '\t' = 1 + countFirstUselessWords xs
  | otherwise = 0

getTokensInternal :: String -> RowColumn -> [LexerResult]
getTokensInternal code rc
  | Success Token {tag = EOF} <- result = [result]
  | Success token <- result =
    let Token {value = v, rowColumn = nRc} = token
        newCode = drop (length v + countFirstUselessWords code) code
     in result : getTokensInternal newCode nRc
  | Error _ _ <- result = [result]
  | Skip v nRc <- result =
    let newCode = drop (length v + countFirstUselessWords code) code
     in getTokensInternal newCode nRc
  | otherwise = [result]
  where
    result = initialState code rc

getTokens :: String -> [LexerResult]
getTokens code = getTokensInternal code $ RowColumn 1 1