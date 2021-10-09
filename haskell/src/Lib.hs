module Lib (compile, runHipoMachine) where

import Codegen (Instruction (Instruction, argument1, operator))
import qualified Data.Map as Map
import GHC.IO.Handle.FD (openFile)
import qualified HipoMachine as HM
import Lexer (LexerResult (Error, Success), RowColumn (RowColumn), Token (Token), getTokens)
import Semantic (SemanticData (currentType, genCode, symbolTable))
import qualified Semantic
import Syntactic (AnalyzerResult, programa)
import qualified Syntactic
import System.Environment (getArgs)
import System.IO (readFile)

run :: String -> AnalyzerResult
run x = (programa . lexerToTokens . getTokens) x Semantic.empty

transformInstruction :: Instruction -> String
transformInstruction x =
  operator ++ " " ++ show arg1
  where
    Instruction {operator = op, argument1 = arg1} = x
    operator = operatorToString op
    operatorToString x = show op

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
          mapM_ (putStrLn . transformInstruction) (genCode sd)
        Syntactic.Error ae -> print ae

lexerToTokens :: [LexerResult] -> [Token]
lexerToTokens [] = []
lexerToTokens (x : xs)
  | Success token <- x = token : lexerToTokens xs
  | Error a _ <- x = error a
  | otherwise = []

runHipoMachine :: String -> IO ()
runHipoMachine = HM.run