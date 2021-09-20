module Syntactic where

import Lexer (NumberType (Float, Integer), RawProgram (tokens), RowColumn (RowColumn), Token (Token, rowColumn, tag, value), TokenType (EOF, Identifier, KeyWord, Number, Symbol))

data SyntacticError = FoundError String RowColumn | ExpectedAnyToken deriving (Show)

data SyntacticResult = Success [Token] | Error SyntacticError deriving (Show)

assertValue esperado obtido = "Esperado: " ++ esperado ++ " obtido: " ++ obtido

tokenTypeToError :: TokenType -> String
tokenTypeToError tokenType = case tokenType of
  Symbol -> "Esperado um simbolo"
  KeyWord -> "Esperado uma palavra reservada"
  Identifier -> "Esperado um identificador"
  EOF -> "Esperado final do arquivo"
  Number _ -> "Esperado um numero"

transformValidade :: ([Token] -> (Bool, String)) -> [Token] -> SyntacticResult
transformValidade f token =
  if isValid
    then Success $ tail token
    else Error $ FoundError error rc
  where
    (isValid, error) = f token
    Token {rowColumn = rc} = head token

validadeValue :: String -> [Token] -> SyntacticResult
validadeValue value = transformValidade (\(Token {value = tValue} : xs) -> (value == tValue, assertValue value tValue))

validadeTag :: TokenType -> [Token] -> SyntacticResult
validadeTag tag = transformValidade (\(Token {tag = tTag, value = v} : xs) -> (tag == tTag, tokenTypeToError tag ++ " encontrado: " ++ v))

validadeSyntactic :: [[Token] -> SyntacticResult] -> [Token] -> SyntacticResult
validadeSyntactic [] tokens = Success tokens
validadeSyntactic _ [] = Success []
validadeSyntactic (x : xs) tokens
  | Success tokens <- result = validadeSyntactic xs tokens
  | Error {} <- result = result
  where
    result = x tokens

tipoVar :: [Token] -> SyntacticResult
tipoVar [] = Error ExpectedAnyToken
tipoVar (x : xs)
  | v == "real" = Success xs
  | v == "integer" = Success xs
  | otherwise = Error $ FoundError ("Esperado identificador real ou inteiro, encontrado: " ++ v) rc
  where
    Token {value = v, rowColumn = rc} = x

pFalsa :: [Token] -> SyntacticResult
pFalsa [] = Error ExpectedAnyToken
pFalsa (x : xs)
  | v == "else" =
    comandos
      xs
  | otherwise = Success $ x : xs
  where
    Token {value = v, rowColumn = rc, tag = t} = x

maisFatores :: [Token] -> SyntacticResult
maisFatores [] = Error ExpectedAnyToken
maisFatores (x : xs)
  | v == "*" || v == "/" =
    validadeSyntactic
      [ fator,
        maisFatores
      ]
      xs
  | otherwise = Success $ x : xs
  where
    Token {value = v, rowColumn = rc, tag = t} = x

outrosTermos :: [Token] -> SyntacticResult
outrosTermos [] = Error ExpectedAnyToken
outrosTermos (x : xs)
  | v == "+" || v == "-" =
    validadeSyntactic
      [ termo,
        outrosTermos
      ]
      xs
  | otherwise = Success $ x : xs
  where
    Token {value = v, rowColumn = rc, tag = t} = x

fator :: [Token] -> SyntacticResult
fator [] = Error ExpectedAnyToken
fator (x : xs)
  | t == Identifier = Success xs
  | t == Number Integer = Success xs
  | t == Number Float = Success xs
  | v == "(" =
    validadeSyntactic
      [ expressao,
        validadeValue ")"
      ]
      xs
  | otherwise = Error $ FoundError "Esperado um fator" rc
  where
    Token {value = v, rowColumn = rc, tag = t} = x

opUn :: [Token] -> SyntacticResult
opUn [] = Error ExpectedAnyToken
opUn (x : xs)
  | v == "-" = Success xs
  | otherwise = Success $ x : xs
  where
    Token {value = v, rowColumn = rc} = x

termo :: [Token] -> SyntacticResult
termo =
  validadeSyntactic
    [ opUn,
      fator,
      maisFatores
    ]

expressao :: [Token] -> SyntacticResult
expressao =
  validadeSyntactic
    [ termo,
      outrosTermos
    ]

relacao :: [Token] -> SyntacticResult
relacao [] = Error ExpectedAnyToken
relacao (x : xs) =
  if v `elem` ["<", ">", "=", "<>", ">=", "<="]
    then Success xs
    else Error $ FoundError "Esperado simbolo de comparação" rc
  where
    Token {value = v, rowColumn = rc} = x

condicao :: [Token] -> SyntacticResult
condicao =
  validadeSyntactic
    [ expressao,
      relacao,
      expressao
    ]

comando :: [Token] -> SyntacticResult
comando [] = Error ExpectedAnyToken
comando (x : xs)
  | v == "read" || v == "write" =
    validadeSyntactic
      [ validadeValue "(",
        validadeTag Identifier,
        validadeValue ")"
      ]
      xs
  | t == Identifier =
    validadeSyntactic
      [ validadeValue ":=",
        expressao
      ]
      xs
  | v == "if" =
    validadeSyntactic
      [ condicao,
        validadeValue "then",
        comandos,
        pFalsa,
        validadeValue "$"
      ]
      xs
  | otherwise = Error $ FoundError ("Comando invalido: " ++ v) rc
  where
    Token {value = v, rowColumn = rc, tag = t} = x

maisComandos :: [Token] -> SyntacticResult
maisComandos [] = Error ExpectedAnyToken
maisComandos (x : xs)
  | v == ";" = comandos xs
  | otherwise = Success $ x : xs
  where
    Token {value = v, rowColumn = rc} = x

comandos :: [Token] -> SyntacticResult
comandos =
  validadeSyntactic
    [ comando,
      maisComandos
    ]

maisVar :: [Token] -> SyntacticResult
maisVar [] = Error ExpectedAnyToken
maisVar (x : xs)
  | v == "," = variaveis xs
  | otherwise = Success $ x : xs
  where
    Token {value = v, rowColumn = rc} = x

variaveis :: [Token] -> SyntacticResult
variaveis =
  validadeSyntactic
    [ validadeTag Identifier,
      maisVar
    ]

dcV :: [Token] -> SyntacticResult
dcV =
  validadeSyntactic
    [ tipoVar,
      validadeValue ":",
      variaveis
    ]

maisDc :: [Token] -> SyntacticResult
maisDc [] = Error ExpectedAnyToken
maisDc (x : xs)
  | v == ";" = dc xs
  | otherwise = Success $ x : xs
  where
    Token {value = v, rowColumn = rc} = x

dc :: [Token] -> SyntacticResult
dc [] = Error ExpectedAnyToken
dc (x : xs)
  | v == "real" || v == "integer" =
    validadeSyntactic
      [ dcV,
        maisDc
      ]
      $ x : xs
  | otherwise = Success $ x : xs
  where
    Token {value = v, rowColumn = rc} = x

corpo :: [Token] -> SyntacticResult
corpo =
  validadeSyntactic
    [ dc,
      validadeValue "begin",
      comandos,
      validadeValue "end"
    ]

programa :: [Token] -> SyntacticResult
programa =
  validadeSyntactic
    [ validadeValue "program",
      validadeTag Identifier,
      corpo,
      validadeValue "."
    ]