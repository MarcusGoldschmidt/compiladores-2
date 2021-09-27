module Codegen where

import qualified Lexer as Lex

data Operador
  = ALME
  | READ
  | WRITE
  | JF
  | GOTO
  | PARA
  | ASSIGNMENT
  | MATH String
  | COMPARE String
  | AWAITING
  deriving (Show, Eq)

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
    argument1 :: String,
    argument2 :: String,
    result :: Result,
    meta :: Maybe MetaData
  }
  deriving (Show, Eq)

variable :: String -> Lex.NumberType -> Instruction
variable name t = Instruction ALME firstArg "" (Result name) Nothing
  where
    firstArg = case t of
      Lex.Float -> "0.0"
      Lex.Integer -> "0"

emptyInstructionWithParameterAndResult :: String -> String -> String -> Instruction
emptyInstructionWithParameterAndResult a b r = Instruction AWAITING a b (Result r) $ Just $ MetaData "temp"

functionCall :: String -> String -> Instruction
functionCall fName variable = Instruction operator variable "" (Result "") Nothing
  where
    operator = case (fName, fName `elem` mathOperators) of
      ("read", _) -> READ
      ("write", _) -> WRITE
      (_, True) -> MATH fName
      _ -> error $ "Nao foi encontrado operador: " ++ fName

partialFunctionCall :: String -> String -> Instruction
partialFunctionCall fName = openInstruction operator
  where
    operator =
      if fName `elem` mathOperators
        then MATH fName
        else error $ "Nao foi encontrado operador: " ++ fName

openInstruction :: Operador -> String -> Instruction
openInstruction op result = Instruction op "" "" (Result result) $ Just $ MetaData "temp"

openTempMathInstruction :: String -> String -> Instruction
openTempMathInstruction op result = Instruction (MATH op) "" "" (Result result) $ Just $ MetaData "temp"