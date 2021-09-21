module Semantic where

import qualified Data.Map as Map
import Lexer (NumberType)

data SemanticData = SemanticData
  { symbolTable :: Map.Map String NumberType,
    currentType :: Maybe NumberType
  }
  deriving (Show)

empty :: SemanticData
empty = Semantic.SemanticData {symbolTable = Map.empty, currentType = Nothing}