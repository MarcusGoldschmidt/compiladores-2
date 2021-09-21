module Syntactic where

import Lexer (NumberType (Float, Integer), RowColumn (RowColumn), Token (Token, rowColumn, tag, value), TokenType (EOF, Identifier, KeyWord, Number, Symbol))
import Semantic (SemanticData)

data SyntacticError = FoundError String RowColumn | ExpectedAnyToken deriving (Show)

data SyntacticResult = Success [Token] SemanticData | Error SyntacticError deriving (Show)

assertValue esperado obtido = "Esperado: " ++ esperado ++ " obtido: " ++ obtido

tokenTypeToError :: TokenType -> String
tokenTypeToError tokenType = case tokenType of
  Symbol -> "Esperado um simbolo"
  KeyWord -> "Esperado uma palavra reservada"
  Identifier -> "Esperado um identificador"
  EOF -> "Esperado final do arquivo"
  Number _ -> "Esperado um numero"

transformValidade :: ([Token] -> (Bool, String)) -> [Token] -> SemanticData -> SyntacticResult
transformValidade f token sd =
  if isValid
    then Success (tail token) sd
    else Error $ FoundError error rc
  where
    (isValid, error) = f token
    Token {rowColumn = rc} = head token

validadeValue :: String -> [Token] -> SemanticData -> SyntacticResult
validadeValue value = transformValidade (\(Token {value = tValue} : xs) -> (value == tValue, assertValue value tValue))

validadeTag :: TokenType -> [Token] -> SemanticData -> SyntacticResult
validadeTag tag = transformValidade (\(Token {tag = tTag, value = v} : xs) -> (tag == tTag, tokenTypeToError tag ++ " encontrado: " ++ v))

validadeSyntactic :: [[Token] -> SemanticData -> SyntacticResult] -> [Token] -> SemanticData -> SyntacticResult
validadeSyntactic [] tokens sd = Success tokens sd
validadeSyntactic (f : fs) tokens sd
  | Success tokens sd <- result = validadeSyntactic fs tokens sd
  | Error {} <- result = result
  where
    result = f tokens sd

tipoVar :: [Token] -> SemanticData -> SyntacticResult
tipoVar [] _ = Error ExpectedAnyToken
tipoVar (x : xs) sd
  | v == "real" = Success xs sd
  | v == "integer" = Success xs sd
  | otherwise = Error $ FoundError ("Esperado identificador real ou inteiro, encontrado: " ++ v) rc
  where
    Token {value = v, rowColumn = rc} = x

pFalsa :: [Token] -> SemanticData -> SyntacticResult
pFalsa [] _ = Error ExpectedAnyToken
pFalsa (x : xs) sd
  | v == "else" = comandos xs sd
  | otherwise = Success (x : xs) sd 
  where
    Token {value = v, rowColumn = rc, tag = t} = x

maisFatores :: [Token] -> SemanticData -> SyntacticResult
maisFatores [] _ = Error ExpectedAnyToken
maisFatores (x : xs) sd
  | v == "*" || v == "/" =
    validadeSyntactic
      [ fator,
        maisFatores
      ]
      xs
      sd
  | otherwise = Success (x : xs) sd
  where
    Token {value = v, rowColumn = rc, tag = t} = x

outrosTermos :: [Token] -> SemanticData -> SyntacticResult
outrosTermos [] _ = Error ExpectedAnyToken
outrosTermos (x : xs) sd
  | v == "+" || v == "-" =
    validadeSyntactic
      [ termo,
        outrosTermos
      ]
      xs
      sd
  | otherwise = Success (x : xs) sd
  where
    Token {value = v, rowColumn = rc, tag = t} = x

fator :: [Token] -> SemanticData -> SyntacticResult
fator [] _ = Error ExpectedAnyToken
fator (x : xs) sd
  | t == Identifier = Success xs sd
  | t == Number Integer = Success xs sd
  | t == Number Float = Success xs sd
  | v == "(" =
    validadeSyntactic
      [ expressao,
        validadeValue ")"
      ]
      xs
      sd
  | otherwise = Error $ FoundError "Esperado um fator" rc
  where
    Token {value = v, rowColumn = rc, tag = t} = x

opUn :: [Token] -> SemanticData -> SyntacticResult
opUn [] _ = Error ExpectedAnyToken
opUn (x : xs) sd
  | v == "-" = Success xs sd
  | otherwise = Success (x : xs) sd
  where
    Token {value = v, rowColumn = rc} = x

termo :: [Token] -> SemanticData -> SyntacticResult
termo =
  validadeSyntactic
    [ opUn,
      fator,
      maisFatores
    ]

expressao :: [Token] -> SemanticData -> SyntacticResult
expressao =
  validadeSyntactic
    [ termo,
      outrosTermos
    ]

relacao :: [Token] -> SemanticData -> SyntacticResult
relacao [] _ = Error ExpectedAnyToken
relacao (x : xs) sd =
  if v `elem` ["<", ">", "=", "<>", ">=", "<="]
    then Success xs sd
    else Error $ FoundError "Esperado simbolo de comparação" rc
  where
    Token {value = v, rowColumn = rc} = x

condicao :: [Token] -> SemanticData -> SyntacticResult
condicao =
  validadeSyntactic
    [ expressao,
      relacao,
      expressao
    ]

comando :: [Token] -> SemanticData -> SyntacticResult
comando [] _ = Error ExpectedAnyToken
comando (x : xs) sd
  | v == "read" || v == "write" =
    validadeSyntactic
      [ validadeValue "(",
        validadeTag Identifier,
        validadeValue ")"
      ]
      xs
      sd
  | t == Identifier =
    validadeSyntactic
      [ validadeValue ":=",
        expressao
      ]
      xs
      sd
  | v == "if" =
    validadeSyntactic
      [ condicao,
        validadeValue "then",
        comandos,
        pFalsa,
        validadeValue "$"
      ]
      xs
      sd
  | otherwise = Error $ FoundError ("Comando invalido: " ++ v) rc
  where
    Token {value = v, rowColumn = rc, tag = t} = x

maisComandos :: [Token] -> SemanticData -> SyntacticResult
maisComandos [] _ = Error ExpectedAnyToken
maisComandos (x : xs) sd
  | v == ";" = comandos xs sd
  | otherwise = Success (x : xs) sd
  where
    Token {value = v, rowColumn = rc} = x

comandos :: [Token] -> SemanticData -> SyntacticResult
comandos =
  validadeSyntactic
    [ comando,
      maisComandos
    ]

maisVar :: [Token] -> SemanticData -> SyntacticResult
maisVar [] _ = Error ExpectedAnyToken
maisVar (x : xs) sd
  | v == "," = variaveis xs sd
  | otherwise = Success (x : xs) sd
  where
    Token {value = v, rowColumn = rc} = x

variaveis :: [Token] -> SemanticData -> SyntacticResult
variaveis =
  validadeSyntactic
    [ validadeTag Identifier,
      maisVar
    ]

dcV :: [Token] -> SemanticData -> SyntacticResult
dcV =
  validadeSyntactic
    [ tipoVar,
      validadeValue ":",
      variaveis
    ]

maisDc :: [Token] -> SemanticData -> SyntacticResult
maisDc [] _ = Error ExpectedAnyToken
maisDc (x : xs) sd
  | v == ";" = dc xs sd
  | otherwise = Success (x : xs) sd
  where
    Token {value = v, rowColumn = rc} = x

dc :: [Token] -> SemanticData -> SyntacticResult
dc [] _ = Error ExpectedAnyToken
dc (x : xs) sd
  | v == "real" || v == "integer" =
    validadeSyntactic
      [ dcV,
        maisDc
      ]
      (x : xs)
      sd
  | otherwise = Success (x : xs) sd
  where
    Token {value = v, rowColumn = rc} = x

corpo :: [Token] -> SemanticData -> SyntacticResult
corpo =
  validadeSyntactic
    [ dc,
      validadeValue "begin",
      comandos,
      validadeValue "end"
    ]

programa :: [Token] -> SemanticData -> SyntacticResult
programa =
  validadeSyntactic
    [ validadeValue "program",
      validadeTag Identifier,
      corpo,
      validadeValue ".",
      validadeTag EOF
    ]