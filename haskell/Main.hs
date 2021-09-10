module Main where

import Lexer

main :: IO ()
main = do print (show $ getTokensInternal "begin teste program" $ RowColumn 0 0)
