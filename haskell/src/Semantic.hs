module Semantic where

import Codegen (Instruction (Instruction, argument1, meta, operator), MetaData (MetaData), Operador (INPP, PARA), Result (Result), openInstruction)
import qualified Codegen as Gen
import Data.List (find)
import qualified Data.List as L
import Data.Map ((!))
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Lexer as Lex
import qualified Text.Read as R

data ProgramType = Boolean | Number Lex.NumberType deriving (Show, Eq)

data IdentifierType = ProgramName | Variable Int ProgramType deriving (Show)

data SemanticData = SemanticData
  { symbolTable :: Map.Map String IdentifierType,
    currentType :: Maybe ProgramType,
    genCode :: [Instruction]
  }
  deriving (Show)

getIdentifierType :: Lex.Token -> SemanticData -> Maybe IdentifierType
getIdentifierType token SemanticData {currentType = Nothing} = Just ProgramName
getIdentifierType token sd = Just $ Variable (Map.size st - 1) variableType
  where
    SemanticData {currentType = Just variableType, symbolTable = st} = sd

empty :: SemanticData
empty = SemanticData {symbolTable = Map.empty, currentType = Nothing, genCode = []}

appendCodeGen :: Instruction -> SemanticData -> SemanticData
appendCodeGen ins sd = sd {genCode = newList}
  where
    newList = genCode sd ++ [ins]

appendCodesGen :: [Instruction] -> SemanticData -> SemanticData
appendCodesGen ins sd = sd {genCode = newList}
  where
    newList = genCode sd ++ ins

replaceLastCodeGen :: Instruction -> SemanticData -> SemanticData
replaceLastCodeGen ins sd = sd {genCode = newList}
  where
    newList = init (genCode sd) ++ [ins]

addInpp :: SemanticData -> SemanticData
addInpp sd = sd {genCode = newList}
  where
    list = genCode sd
    newList = list ++ [openInstruction INPP 0]

addStop :: SemanticData -> SemanticData
addStop sd = sd {genCode = newList}
  where
    list = genCode sd
    newList = list ++ [openInstruction PARA 0]

addMeta :: String -> SemanticData -> SemanticData
addMeta meta sd = sd {genCode = newList}
  where
    lastCode = last $ genCode sd
    newList = init (genCode sd) ++ [lastCode {meta = Just $ MetaData meta}]

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

findVariableIndex :: String -> SemanticData -> Int
findVariableIndex name sd =
  case variable of
    Variable index _ -> index
    _ -> error "Esperado uma variavel"
  where
    variable = symbolTable sd ! name

findLastTag :: String -> SemanticData -> Maybe String
findLastTag tag sd =
  case result of
    Just Instruction {meta = Just (MetaData tag)} -> Just tag
    _ -> Nothing
  where
    compare ins = case ins of
      (Instruction _ _ (Just (MetaData x))) -> x == tag
      _ -> False
    result = find compare $ reverse $ genCode sd

createNewTag :: String -> SemanticData -> SemanticData
createNewTag defaultTag sd =
  sd
  where
    lastag = findLastTag defaultTag sd
    newTag = case lastag of
      Just tag -> defaultTag ++ show ((read (drop (length defaultTag) tag) :: Integer) + 1)
      Nothing -> defaultTag ++ "1"

createNewFatorTag :: SemanticData -> SemanticData
createNewFatorTag = createNewTag "fator"