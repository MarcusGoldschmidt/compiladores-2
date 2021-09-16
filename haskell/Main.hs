module Main where

import Lexer (RowColumn (RowColumn), getTokensInternal)
import Syntactic ()

main :: IO ()
main = do print (show $ getTokensInternal "begin teste program" $ RowColumn 0 0)
