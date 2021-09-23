module Semantic where

import qualified Data.Map as Map
import Lexer (NumberType, Token (Token, tag), TokenType (Number))

data IdentifierType = ProgramName | Variable NumberType deriving (Show)

data SemanticData = SemanticData
  { symbolTable :: Map.Map String IdentifierType,
    currentType :: Maybe NumberType
  }
  deriving (Show)

getIdentifierType :: Token -> SemanticData -> Maybe IdentifierType
getIdentifierType token SemanticData {currentType = Nothing} = Just ProgramName
getIdentifierType Token {tag = (Number variableType)} SemanticData {currentType = Just a} = Just $ Variable variableType
getIdentifierType _ _ = Nothing

empty :: SemanticData
empty = SemanticData {symbolTable = Map.empty, currentType = Nothing}
