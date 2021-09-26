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

  let a = (lexerToTokens . getTokens) "program teste real: a,b,c,d; begin a := 2.0 * a + a end."

  print a