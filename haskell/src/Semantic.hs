module Semantic where

import Codegen (Instruction (Instruction, argument1, argument2, meta, operator, result), MetaData (MetaData), Operador (AWAITING, PARA), Result (Result), openInstruction, openTempMathInstruction)
import qualified Codegen as Gen
import Data.List (find)
import qualified Data.List as L
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Lexer as Lex
import qualified Text.Read as R

data ProgramType = Boolean | Number Lex.NumberType deriving (Show, Eq)

data IdentifierType = ProgramName | Variable ProgramType deriving (Show)

data SemanticData = SemanticData
  { symbolTable :: Map.Map String IdentifierType,
    currentType :: Maybe ProgramType,
    genCode :: [Instruction]
  }
  deriving (Show)

getIdentifierType :: Lex.Token -> SemanticData -> Maybe IdentifierType
getIdentifierType Lex.Token {Lex.tag = (Lex.Number variableType)} _ = Just $ Variable $ Number variableType
getIdentifierType token SemanticData {currentType = Nothing} = Just ProgramName
getIdentifierType token SemanticData {currentType = Just variableType} = Just $ Variable variableType

empty :: SemanticData
empty = SemanticData {symbolTable = Map.empty, currentType = Nothing, genCode = []}

appendCodeGen :: Instruction -> SemanticData -> SemanticData
appendCodeGen ins sd = sd {genCode = newList}
  where
    newList = genCode sd ++ [ins]

replaceLastCodeGen :: Instruction -> SemanticData -> SemanticData
replaceLastCodeGen ins sd = sd {genCode = newList}
  where
    newList = init (genCode sd) ++ [ins]

isTempVariabel :: Instruction -> Bool
isTempVariabel x =
  T.isPrefixOf (T.pack "t") (T.pack t) && case meta of
    Just a -> "temp" == getData a
    _ -> False
  where
    Instruction _ _ _ (Result t) meta = x
    getData (MetaData metaValue) = metaValue

currentTempVar :: SemanticData -> String
currentTempVar sd =
  case result of
    Just (Instruction _ _ _ (Result t) _) -> t
    Nothing -> "t1"
  where
    instrucions = genCode sd
    result = L.find isTempVariabel $ reverse instrucions

nextTempVar :: SemanticData -> String
nextTempVar sd =
  case result of
    Just (Instruction _ _ _ (Result t) _) -> head t : show ((read (tail t) :: Int) + 1)
    Nothing -> "t1"
  where
    instrucions = genCode sd
    result = L.find isTempVariabel $ reverse instrucions

addFirstArgumentLastInstruction :: String -> [Instruction] -> [Instruction]
addFirstArgumentLastInstruction arg ins = listWihoutLastElement ++ [lastIns {argument1 = arg}]
  where
    lastIns = last ins
    listWihoutLastElement = init ins

addSecondArgumentLastInstruction :: String -> [Instruction] -> [Instruction]
addSecondArgumentLastInstruction arg ins = listWihoutLastElement ++ [lastIns {argument2 = arg}]
  where
    lastIns = last ins
    listWihoutLastElement = init ins

addTempMathInstruction :: String -> SemanticData -> SemanticData
addTempMathInstruction op sd = appendCodeGen (Gen.Instruction (Gen.MATH op) currentTemp "" (Gen.Result nextTemp) $ Just $ MetaData "temp") sd
  where
    nextTemp = nextTempVar sd
    currentTemp = currentTempVar sd

mergeTempMathInstruction :: String -> SemanticData -> SemanticData
mergeTempMathInstruction op sd = sd {genCode = newList}
  where
    currentTemp = currentTempVar sd
    currentList = genCode sd
    lastCode = last currentList
    newList = init currentList ++ [(openTempMathInstruction op currentTemp) {result = result lastCode, argument1 = argument1 lastCode}]

addStop :: SemanticData -> SemanticData
addStop sd = sd {genCode = newList}
  where
    list = genCode sd
    newList = list ++ [openInstruction PARA ""]

addMeta :: String -> SemanticData -> SemanticData
addMeta meta sd = sd {genCode = newList}
  where
    lastCode = last $ genCode sd
    newList = init (genCode sd) ++ [lastCode {meta = Just $ MetaData meta}]

findVariableByMeta :: String -> SemanticData -> (String, SemanticData)
findVariableByMeta tag sd = case ins of
  Just a -> case a of
    ins@Instruction {operator = AWAITING, argument1 = a} -> (a, sd {genCode = remove ins $ genCode sd})
    Instruction {result = (Result a)} -> (a, sd)
  Nothing -> error "Meta tag nao encontrada"
  where
    list = reverse $ genCode sd
    compare x = case meta x of
      Just a -> a == MetaData tag
      _ -> False
    ins = find compare list
    remove element list = filter (/= element) list

findInstructionByMeta :: String -> SemanticData -> Instruction
findInstructionByMeta tag sd = case ins of
  Just a -> a
  Nothing -> error "Meta tag nao encontrada"
  where
    list = reverse $ genCode sd
    compare x = case meta x of
      Just a -> a == MetaData tag
      _ -> False
    ins = find compare list