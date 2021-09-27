module Lib (compile) where

import Codegen
import GHC.IO.Handle.FD (openFile)
import Lexer (LexerResult (Error, Success), RowColumn (RowColumn), Token (Token), getTokens)
import Semantic (SemanticData (currentType, genCode, symbolTable))
import qualified Semantic
import Syntactic (AnalyzerResult, programa)
import qualified Syntactic
import System.Environment (getArgs)
import System.IO (readFile)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

run :: String -> AnalyzerResult
run x = (programa . lexerToTokens . getTokens) x Semantic.empty

transformInstruction :: Instruction -> String
transformInstruction x =
  operator ++ " " ++ arg1 ++ " " ++ arg2 ++ " " ++ r
  where
    Instruction {operator = op, argument1 = arg1, argument2 = arg2, result = (Result r)} = x
    operator = operatorToString op
    operatorToString x = case op of
      MATH a -> a
      COMPARE a -> a
      ASSIGNMENT -> ":="
      _ -> show op

compile :: IO ()
compile = do
  args <- getArgs
  case args of
    [] -> print "Faltou informar o arquivo"
    x : xs -> do
      code <- readFile x
      let result = run code
      case result of
        Syntactic.Success tos sd -> do
          mapM_ (print . transformInstruction) (genCode sd)
        Syntactic.Error ae -> print ae

lexerToTokens :: [LexerResult] -> [Token]
lexerToTokens [] = []
lexerToTokens (x : xs)
  | Success token <- x = token : lexerToTokens xs
  | Error a _ <- x = error a
  | otherwise = []