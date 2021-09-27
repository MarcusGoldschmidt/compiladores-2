import qualified Data.Map as Map
import Lexer
import Semantic (SemanticData (currentType, symbolTable))
import qualified Semantic as Sem
import Syntactic (addVariable, semanticPipeline)
import qualified Syntactic as Sem
import qualified Syntactic as Syn
import Test.QuickCheck.Test (Result (Success))

compareError as
  | Syn.Error _ <- as = False
  | otherwise = True

lexerToTokens :: [LexerResult] -> [Token]
lexerToTokens [] = []
lexerToTokens (x : xs)
  | Lexer.Success token <- x = token : lexerToTokens xs
  | Lexer.Error a _ <- x = error a
  | otherwise = []

main :: IO ()
main = do

  let a = (lexerToTokens . getTokens) "if a <= b then if b <= a then if a = b then write(a)$$$;"

  mapM_ print a

  print ""