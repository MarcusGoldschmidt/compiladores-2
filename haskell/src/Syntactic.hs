module Syntactic where

import Data.Map (insert, member)
import qualified Data.Map as Map
import Data.Maybe (isJust, isNothing)
import qualified Data.Maybe
import Lexer (LexerResult, NumberType (Float, Integer), RowColumn (RowColumn), Token (Token, rowColumn, tag, value), TokenType (EOF, Identifier, KeyWord, Number, Symbol))
import Semantic (IdentifierType (ProgramName, Variable), ProgramType, SemanticData (SemanticData, currentType, symbolTable), getIdentifierType)
import qualified Semantic as Sem
import qualified Semantic as Sen
import Utils (head', tail1)

data AnalyzerError = FoundError String RowColumn | ExpectedAnyToken deriving (Show)

data AnalyzerResult = Success [Token] SemanticData | Error AnalyzerError deriving (Show)

addVariable :: AnalyzerResult -> AnalyzerResult
addVariable s@(Success tks@(x@Token {tag = Identifier} : _) sd) =
  if member v st
    then Error $ FoundError ("Variavel \"" ++ v ++ "\" ja foi declarada") rc
    else case numberType of
      Just a -> Success tks $ sd {symbolTable = insert v a st}
      Nothing -> Error $ FoundError (v ++ " nao e um tipo valido") rc
  where
    SemanticData {symbolTable = st} = sd
    Token {value = v, tag = t, rowColumn = rc} = x
    numberType = getIdentifierType x sd
addVariable s = s

addCurrentType :: ProgramType -> [Token] -> SemanticData -> AnalyzerResult
addCurrentType pt tks sd = Success tks $ sd {currentType = Just pt}

removeType :: [Token] -> SemanticData -> AnalyzerResult
removeType tks sd = Success tks $ sd {currentType = Nothing}

verifyIfIdentifierExist :: [Token] -> SemanticData -> AnalyzerResult
verifyIfIdentifierExist tks@(x : xs) sd = case result of
  Just a -> case a of
    ProgramName -> Error $ FoundError "Nome do programa nao pode ser usado" rc
    _ -> Success tks sd
  Nothing -> Error $ FoundError ("Variavel \"" ++ v ++ "\" nao declarada") rc
  where
    SemanticData {symbolTable = st} = sd
    Token {value = v, rowColumn = rc} = x
    result = Map.lookup v st
verifyIfIdentifierExist tks sd = Success tks sd

setCurrentTypeIfNotExist :: [Token] -> SemanticData -> AnalyzerResult
setCurrentTypeIfNotExist tks@(x : xs) sd =
  if isNothing currentType
    then case tag x of
      Identifier -> case variableType of
        Just a -> case a of
          Sem.Variable numberType -> Success tks $ sd {currentType = Just numberType}
          _ -> Error $ FoundError "Nao foi encontrado tipo" rc
        _ -> Error $ FoundError ("Variavel " ++ v ++ " nao encontrada") rc
      Number numberType -> Success tks $ sd {currentType = Just $ Sem.Number numberType}
      _ -> Error $ FoundError "Nao foi encontrado tipo" rc
    else Success tks sd
  where
    v = value x
    rc = rowColumn x
    SemanticData {symbolTable = symbolTable, currentType = currentType} = sd
    variableType = Map.lookup v symbolTable
setCurrentTypeIfNotExist tks sd = Success tks sd

verifyCurrentType :: [Token] -> SemanticData -> AnalyzerResult
verifyCurrentType (x : xs) sd =
  case (tokenType, currentType) of
    (Just a, Just b) -> case a of
      Variable pt ->
        if pt == b
          then Success (x : xs) sd
          else Error $ FoundError ("Foi encontrado: " ++ show pt ++ " esperado: " ++ show b) rc
      _ -> Error $ FoundError "Nao foi encontrado tipagem para essa expressao" rc
    (_, _) -> Error $ FoundError "Nao foi encontrado tipagem para essa expressao" rc
  where
    SemanticData {currentType = currentType, symbolTable = symbolTable} = sd
    rc = rowColumn x
    tokenType = case tag x of
      Identifier -> Map.lookup (value x) symbolTable
      Number numberType -> Just $ Sem.Variable $ Sem.Number numberType
      _ -> Nothing
verifyCurrentType _ _ = Error ExpectedAnyToken

setCurrentTypeForVariableType :: [Token] -> SemanticData -> AnalyzerResult
setCurrentTypeForVariableType tks@(x : xs) sd =
  case result of
    Just a -> case a of
      ProgramName -> Error $ FoundError "Nome do programa nao pode ser usado" rc
      Variable (Sem.Number nt) -> Success tks $ sd {currentType = Just $ Sem.Number nt}
      _ -> Error $ FoundError "Erro inesperado" rc
    Nothing -> Error $ FoundError ("Variavel \"" ++ v ++ "\" nao declarada") rc
  where
    SemanticData {symbolTable = st} = sd
    Token {tag = Identifier, value = v, rowColumn = rc} = x
    result = Map.lookup v st
setCurrentTypeForVariableType tks sd = Success tks sd

skipToken :: [Token] -> SemanticData -> AnalyzerResult
skipToken (x : xs) sd = Success xs sd
skipToken tks sd = Success tks sd

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

addSemanticValidation :: (AnalyzerResult -> AnalyzerResult) -> [Token] -> SemanticData -> AnalyzerResult
addSemanticValidation f tks sd = f $ Success tks sd

validadeAfter :: ([Token] -> SemanticData -> AnalyzerResult) -> ([Token] -> SemanticData -> AnalyzerResult) -> [Token] -> SemanticData -> AnalyzerResult
validadeAfter f1 f2 tks sd =
  case result1 of
    Success ntks nsd ->
      case result2 of
        Success _ nsd2 -> Success ntks nsd2
        Error _ -> result2
      where
        result2 = f2 tks nsd
    Error _ -> result1
  where
    result1 = f1 tks sd

semanticPipeline :: [Token] -> SemanticData -> [AnalyzerResult -> AnalyzerResult] -> Maybe AnalyzerResult
semanticPipeline tks sd = head' . tail1 . dropWhile (not . compareError) . scanl (\acc fun -> fun acc) (Success tks sd)
  where
    compareError as
      | Error _ <- as = False
      | otherwise = True

validadeSemantic :: ([Token] -> SemanticData -> AnalyzerResult) -> [AnalyzerResult -> AnalyzerResult] -> [Token] -> SemanticData -> AnalyzerResult
validadeSemantic f fs tks sd =
  case result of
    (Success tokens semantic) -> case validation of
      Just (Success _ resultSd) -> Success tokens resultSd
      Just (Error a) -> Error a
      _ -> result
    (Error _) -> result
  where
    result = f tks sd
    (Success _ rSd) = result
    validation = semanticPipeline tks rSd fs

tipoVar :: [Token] -> SemanticData -> AnalyzerResult
tipoVar [] _ = Error ExpectedAnyToken
tipoVar (x : xs) sd
  | v == "real" = Success xs $ sd {currentType = Just $ Sem.Number Float}
  | v == "integer" = Success xs $ sd {currentType = Just $ Sem.Number Integer}
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
  | t == Identifier = validadeSyntactic [verifyIfIdentifierExist, setCurrentTypeIfNotExist, verifyCurrentType, skipToken] (x : xs) sd
  | t == Number Integer = validadeSyntactic [setCurrentTypeIfNotExist, verifyCurrentType, skipToken] (x : xs) sd
  | t == Number Float = validadeSyntactic [setCurrentTypeIfNotExist, verifyCurrentType, skipToken] (x : xs) sd
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
        validadeAfter (validadeTag Identifier) verifyIfIdentifierExist,
        validadeValue ")"
      ]
      xs
      sd
  | t == Identifier =
    validadeSyntactic
      [ setCurrentTypeForVariableType,
        skipToken,
        validadeValue ":=",
        expressao
      ]
      (x : xs)
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
      removeType,
      validadeValue "begin",
      comandos,
      validadeValue "end"
    ]

programa :: [Token] -> SemanticData -> AnalyzerResult
programa =
  validadeSyntactic
    [ validadeValue "program",
      addSemanticValidation addVariable,
      validadeTag Identifier,
      corpo,
      validadeValue ".",
      validadeTag EOF
    ]
