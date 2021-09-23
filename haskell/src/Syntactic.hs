module Syntactic where

import Data.Map (insert, member)
import Lexer (NumberType (Float, Integer), RowColumn (RowColumn), Token (Token, rowColumn, tag, value), TokenType (EOF, Identifier, KeyWord, Number, Symbol))
import Semantic (SemanticData (SemanticData, symbolTable), getIdentifierType)

data AnalyzerError = FoundError String RowColumn | ExpectedAnyToken deriving (Show)

data AnalyzerResult = Success [Token] SemanticData | Error AnalyzerError deriving (Show)

addVariable :: AnalyzerResult -> AnalyzerResult
addVariable s@(Success (x@Token {tag = Identifier} : xs) sd) =
  if member v st
    then Error $ FoundError ("Variavel ja " ++ v ++ " declarada") rc
    else case numberType of
      Just a -> Success xs $ sd {symbolTable = insert v a st}
      Nothing -> Error $ FoundError (v ++ " nao e um tipo valido") rc
  where
    SemanticData {symbolTable = st} = sd
    Token {value = v, tag = t, rowColumn = rc} = x
    numberType = getIdentifierType x sd
addVariable s = s

assertValue esperado obtido = "Esperado: " ++ esperado ++ " obtido: " ++ obtido

tokenTypeToError :: TokenType -> String
tokenTypeToError tokenType = case tokenType of
  Symbol -> "Esperado um simbolo"
  KeyWord -> "Esperado uma palavra reservada"
  Identifier -> "Esperado um identificador"
  EOF -> "Esperado final do arquivo"
  Number _ -> "Esperado um numero"

transformValidade :: ([Token] -> (Bool, String)) -> [Token] -> SemanticData -> AnalyzerResult
transformValidade f token sd =
  if isValid
    then Success (tail token) sd
    else Error $ FoundError error rc
  where
    (isValid, error) = f token
    Token {rowColumn = rc} = head token

validadeValue :: String -> [Token] -> SemanticData -> AnalyzerResult
validadeValue value = transformValidade (\(Token {value = tValue} : xs) -> (value == tValue, assertValue value tValue))

validadeTag :: TokenType -> [Token] -> SemanticData -> AnalyzerResult
validadeTag tag = transformValidade (\(Token {tag = tTag, value = v} : xs) -> (tag == tTag, tokenTypeToError tag ++ " encontrado: " ++ v))

validadeSyntactic :: [[Token] -> SemanticData -> AnalyzerResult] -> [Token] -> SemanticData -> AnalyzerResult
validadeSyntactic [] tokens sd = Success tokens sd
validadeSyntactic (f : fs) tokens sd
  | Success tokens sd <- result = validadeSyntactic fs tokens sd
  | Error {} <- result = result
  where
    result = f tokens sd

validadeSemantic :: ([Token] -> SemanticData -> AnalyzerResult) -> [AnalyzerResult -> AnalyzerResult] -> [Token] -> SemanticData -> AnalyzerResult
validadeSemantic f fs tk sd =
  case result of
    (Success tokens semantic) -> validade fs
    (Error _) -> result
  where
    result = f tk sd
    validade = foldl (\acc x -> x acc) result

tipoVar :: [Token] -> SemanticData -> AnalyzerResult
tipoVar [] _ = Error ExpectedAnyToken
tipoVar (x : xs) sd
  | v == "real" = Success xs sd
  | v == "integer" = Success xs sd
  | otherwise = Error $ FoundError ("Esperado identificador real ou inteiro, encontrado: " ++ v) rc
  where
    Token {value = v, rowColumn = rc} = x

pFalsa :: [Token] -> SemanticData -> AnalyzerResult
pFalsa [] _ = Error ExpectedAnyToken
pFalsa (x : xs) sd
  | v == "else" = comandos xs sd
  | otherwise = Success (x : xs) sd
  where
    Token {value = v, rowColumn = rc, tag = t} = x

maisFatores :: [Token] -> SemanticData -> AnalyzerResult
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

outrosTermos :: [Token] -> SemanticData -> AnalyzerResult
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

fator :: [Token] -> SemanticData -> AnalyzerResult
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

opUn :: [Token] -> SemanticData -> AnalyzerResult
opUn [] _ = Error ExpectedAnyToken
opUn (x : xs) sd
  | v == "-" = Success xs sd
  | otherwise = Success (x : xs) sd
  where
    Token {value = v, rowColumn = rc} = x

termo :: [Token] -> SemanticData -> AnalyzerResult
termo =
  validadeSyntactic
    [ opUn,
      fator,
      maisFatores
    ]

expressao :: [Token] -> SemanticData -> AnalyzerResult
expressao =
  validadeSyntactic
    [ termo,
      outrosTermos
    ]

relacao :: [Token] -> SemanticData -> AnalyzerResult
relacao [] _ = Error ExpectedAnyToken
relacao (x : xs) sd =
  if v `elem` ["<", ">", "=", "<>", ">=", "<="]
    then Success xs sd
    else Error $ FoundError "Esperado simbolo de comparação" rc
  where
    Token {value = v, rowColumn = rc} = x

condicao :: [Token] -> SemanticData -> AnalyzerResult
condicao =
  validadeSyntactic
    [ expressao,
      relacao,
      expressao
    ]

comando :: [Token] -> SemanticData -> AnalyzerResult
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

maisComandos :: [Token] -> SemanticData -> AnalyzerResult
maisComandos [] _ = Error ExpectedAnyToken
maisComandos (x : xs) sd
  | v == ";" = comandos xs sd
  | otherwise = Success (x : xs) sd
  where
    Token {value = v, rowColumn = rc} = x

comandos :: [Token] -> SemanticData -> AnalyzerResult
comandos =
  validadeSyntactic
    [ comando,
      maisComandos
    ]

maisVar :: [Token] -> SemanticData -> AnalyzerResult
maisVar [] _ = Error ExpectedAnyToken
maisVar (x : xs) sd
  | v == "," = variaveis xs sd
  | otherwise = Success (x : xs) sd
  where
    Token {value = v, rowColumn = rc} = x

variaveis :: [Token] -> SemanticData -> AnalyzerResult
variaveis =
  validadeSemantic
    ( validadeSyntactic
        [ validadeTag Identifier,
          maisVar
        ]
    )
    [addVariable]

dcV :: [Token] -> SemanticData -> AnalyzerResult
dcV =
  validadeSyntactic
    [ tipoVar,
      validadeValue ":",
      variaveis
    ]

maisDc :: [Token] -> SemanticData -> AnalyzerResult
maisDc [] _ = Error ExpectedAnyToken
maisDc (x : xs) sd
  | v == ";" = dc xs sd
  | otherwise = Success (x : xs) sd
  where
    Token {value = v, rowColumn = rc} = x

dc :: [Token] -> SemanticData -> AnalyzerResult
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

corpo :: [Token] -> SemanticData -> AnalyzerResult
corpo =
  validadeSyntactic
    [ dc,
      validadeValue "begin",
      comandos,
      validadeValue "end"
    ]

programa :: [Token] -> SemanticData -> AnalyzerResult
programa =
  validadeSyntactic
    [ validadeValue "program",
      validadeTag Identifier,
      corpo,
      validadeValue ".",
      validadeTag EOF
    ]