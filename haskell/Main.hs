module Main where

import GHC.IO.Handle.FD (openFile)
import Lexer (LexerResult (Success), RowColumn (RowColumn), Token (Token), getTokens)
import Syntactic (SyntacticResult, programa)
import System.Environment (getArgs)
import System.IO (IOMode (ReadMode), readFile)

run :: String -> SyntacticResult
run = programa . lexerToTokens . getTokens

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