module Main where
import Lib (runHipoMachine)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> print "Faltou informar o arquivo"
    x : xs -> do
      code <- readFile x
      runHipoMachine code
