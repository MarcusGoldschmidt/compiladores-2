module Main where

import qualified Data.Sequence as Map
import GHC.IO.Handle.FD (openFile)
import Lexer (LexerResult (Success), RowColumn (RowColumn), Token (Token), getTokens)
import Semantic (SemanticData (symbolTable, currentType))
import qualified Semantic
import Syntactic (SyntacticResult, programa)
import System.Environment (getArgs)
import System.IO (readFile)


run :: String -> SyntacticResult
run x = (programa . lexerToTokens . getTokens) x Semantic.empty 

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> print "Faltou informar o arquivo"
    x : xs -> do
      code <- readFile x
      let result = run code
      print $ show result

lexerToTokens :: [LexerResult] -> [Token]
lexerToTokens [] = []
lexerToTokens (x : xs)
  | Success token <- x = token : lexerToTokens xs
  | otherwise = []