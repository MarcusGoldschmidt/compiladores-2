module Codegen where

import qualified Lexer as Lex

data Operador
  = INPP
  | ALME
  | PARA
  | LEIT
  | ARMZ
  | CRVL
  | CRCT
  | IMPR
  | SOMA
  | SUBT
  | MULT
  | DIVI
  | INVE
  | -- <
    CPME
  | -- >
    CPMA
  | -- =
    CPIG
  | -- <>
    CDES
  | -- <=
    CPMI
  | -- >=
    CMAI
  | DSVF
  | DSVI
  deriving (Eq, Ord, Enum, Read, Show)

mathOperators =
  [ "*",
    "+",
    "-",
    "/"
  ]

newtype MetaData = MetaData String deriving (Show, Eq)

newtype Result = Result String deriving (Show, Eq)

data Instruction = Instruction
  { operator :: Operador,
    argument1 :: Int,
    meta :: Maybe MetaData
  }
  deriving (Show, Eq)

variable :: Instruction
variable = Instruction ALME 1 Nothing

functionCall :: String -> Int -> [Instruction]
functionCall fName variable =
  case (fName, fName `elem` mathOperators) of
    ("read", _) ->
      [ Instruction LEIT 0 Nothing,
        Instruction ARMZ variable Nothing
      ]
    ("write", _) ->
      [ Instruction CRVL variable Nothing,
        Instruction IMPR 0 Nothing
      ] -- TODO: Math
    _ -> error $ "Nao foi encontrado operador: " ++ fName

loadVariable :: Int -> Instruction
loadVariable index = Instruction CRVL index Nothing

assignmentVariable :: Int -> Instruction
assignmentVariable index = Instruction ARMZ index Nothing

loadValue :: Int -> Instruction
loadValue value = Instruction CRCT value Nothing

openInstruction :: Operador -> Int -> Instruction
openInstruction op arg1 = Instruction op arg1 Nothing

mathOperator :: String -> Instruction
mathOperator v = case v of
  "+" -> Instruction SOMA 0 Nothing
  "-" -> Instruction SUBT 0 Nothing
  "/" -> Instruction DIVI 0 Nothing
  "*" -> Instruction MULT 0 Nothing
  _ -> error "Esperado operador"

isMathOperator :: Instruction -> Bool
isMathOperator ins =
  op `elem` [SOMA .. INVE]
  where
    op = operator ins

matchCompareOperator :: String -> Operador
matchCompareOperator a = case a of
  "<" -> CPME
  ">" -> CPMA
  "=" -> CPIG
  "<>" -> CDES
  "<=" -> CPMI
  ">=" -> CMAI
  _ -> error $ "Operador " ++ a ++ " nao valido"

compareOperator :: String -> Instruction
compareOperator s =
  Instruction op 0 Nothing
  where
    op = matchCompareOperator s

dsvfOperator :: Instruction
dsvfOperator = Instruction DSVF 0 Nothing

dsviOperator :: Int -> Instruction
dsviOperator index = Instruction DSVI index Nothing