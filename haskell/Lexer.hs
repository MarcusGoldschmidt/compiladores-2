module Lexer where

import Data.Char (isLetter, isNumber)

data NumberType = Float | Integer deriving (Show)

data TokenType = KeyWord | Symbol | Identifier | EOF | Number NumberType deriving (Show)

type Row = Int

type Column = Int

data RowColumn = RowColumn Row Column deriving (Show)

data Token = Token
  { value :: String,
    tag :: TokenType,
        rowColumn :: RowColumn
  }
  deriving (Show)

data LexerResult = Success Token | Error String RowColumn deriving (Show)

data RawProgram = RawProgram
  { code :: String,
    currentRowColumn :: RowColumn,
    tokens :: [Token]
  }
  deriving (Show)

keyWords =
  [ "program",
    "ident",
    "begin",
    "real",
    "if",
    "then",
    "end",
    "else",
    "integer"
  ]

stopWords = [" ", "\n", "\t"]

symbols =
  [ "*",
    ";",
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

nextLine :: RowColumn -> RowColumn
nextLine (RowColumn x y) = RowColumn (x + 1) 0

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
  | isNumber x = numberLexer (token ++ [x]) xs $ plusColumn rowColumn
  | otherwise = Error ("Esperado um número, encontrado:" ++ [x]) rowColumn

numberAfterDotLexer :: String -> String -> RowColumn -> LexerResult
numberAfterDotLexer token [] rowColumn = Error "Esperado um número" rowColumn
numberAfterDotLexer token (x : xs) rowColumn
  | isNumber x = numberAfterDotLexer (token ++ [x]) xs $ plusColumn rowColumn
  | isStopWordOrSymbol x = Success $ Token token (Number Float) rowColumn
  | otherwise = Error ("Esperado um número, encontrado:" ++ [x]) rowColumn

wordLexer :: String -> String -> RowColumn -> LexerResult
wordLexer token [] rowColumn = Success $ Token token (defineTokenType token) rowColumn
wordLexer token (x : xs) rowColumn
  | isLetter x = wordLexer (token ++ [x]) xs $ plusColumn rowColumn
  | isStopWordOrSymbol x = Success $ Token token (defineTokenType token) rowColumn
  | otherwise = Error ("Erro sintático: " ++ [x]) rowColumn

symbolLexer :: String -> String -> RowColumn -> LexerResult
symbolLexer token [] rowColumn = Error "Erro sintático" rowColumn
symbolLexer token (x:xs) rowColumn
    | x == '<' && second == '>' = Success $ Token "<>" Symbol rowColumn
    | x == '>' && second == '=' = Success $ Token ">=" Symbol rowColumn
    | x == ':' && second == '=' = Success $ Token ":=" Symbol rowColumn
    | isSymbol x = Success $ Token [x] Symbol rowColumn
    | otherwise = Error "Simbolo não reconhecido" rowColumn
    where
        (second : _) = xs

initialState :: String -> RowColumn -> LexerResult
initialState [] rowColumn = Success $ Token "" EOF rowColumn
initialState list rowColumn
  | isLetter x  = wordLexer "" list rowColumn
  | isNumber x  = numberLexer "" list rowColumn
  | isSymbol x  = symbolLexer "" list rowColumn
  | x == ' '    = initialState xs $ plusColumn rowColumn
  | x == '\n'   = initialState xs $ nextLine rowColumn
  | x == '\t'   = initialState xs $ plusColumn rowColumn
  | otherwise = Error "Erro lexico" rowColumn
  where
    (x : xs) = list

countFirstUselessWords [] = 0
countFirstUselessWords (x:xs)
  | x == ' ' || x == '\n' || x == '\t' = 1 + countFirstUselessWords xs
  | otherwise = 0

getTokensInternal :: String -> RowColumn -> [LexerResult]
getTokensInternal code rc
    | Success Token{tag=EOF} <- result = [result]
    | Success token <- result = let
        Token{value=v, rowColumn=nRc} = token
        newCode = drop (length v + countFirstUselessWords code) code
        in
            result : getTokensInternal newCode nRc
    | Error _ _ <- result = [result]
    | otherwise = [result]
    where
        result = initialState code rc