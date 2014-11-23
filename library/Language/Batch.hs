module Language.Batch where

import qualified Language.Batch.Ast.Positioned as Ast
import qualified Language.Batch.Lexer as Lexer
import qualified Language.Batch.Parser as Parser
import qualified Language.Batch.PrettyPrint as PrettyPrint
import qualified Language.Batch.Token as Token

lex :: String -> [Token.Lexeme]
lex code = Lexer.scanLexemes code

parse :: String -> Ast.Program
parse code = Parser.parse code

generateCodeToFile :: Ast.Program -> FilePath -> IO ()
generateCodeToFile = PrettyPrint.printToFile 
