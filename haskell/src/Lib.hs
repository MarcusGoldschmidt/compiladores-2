module Lib (compile) where

import GHC.IO.Handle.FD (openFile)
import Lexer (LexerResult (Error, Success), RowColumn (RowColumn), Token (Token), getTokens)
import Semantic (SemanticData (currentType, symbolTable))
import qualified Semantic
import Syntactic (AnalyzerResult, programa)
import System.Environment (getArgs)
import System.IO (readFile)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

run :: String -> AnalyzerResult
run x = (programa . lexerToTokens . getTokens) x Semantic.empty

compile :: IO ()
compile = do
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
  | Error a _ <- x = error a
  | otherwise = []