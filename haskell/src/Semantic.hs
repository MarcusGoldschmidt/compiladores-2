module Semantic where

import qualified Data.Map as Map
import qualified Lexer as Lex

data ProgramType = Boolean | Number Lex.NumberType deriving (Show, Eq)

data IdentifierType = ProgramName | Variable ProgramType deriving (Show)

data SemanticData = SemanticData
  { symbolTable :: Map.Map String IdentifierType,
    currentType :: Maybe ProgramType
  }
  deriving (Show)

getIdentifierType :: Lex.Token -> SemanticData -> Maybe IdentifierType
getIdentifierType Lex.Token {Lex.tag = (Lex.Number variableType)} _ = Just $ Variable $ Number variableType
getIdentifierType token SemanticData {currentType = Nothing} = Just ProgramName
getIdentifierType token SemanticData {currentType = Just variableType} = Just $ Variable variableType

empty :: SemanticData
empty = SemanticData {symbolTable = Map.empty, currentType = Nothing}
