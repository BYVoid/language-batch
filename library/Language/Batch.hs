module Language.Batch where

import qualified Language.Batch.Ast.Positioned as Ast
import qualified Language.Batch.Lexer as Lexer
import qualified Language.Batch.Parser as Parser
import qualified Language.Batch.PrettyPrint as PrettyPrint
import qualified Language.Batch.SyntaxTree as SyntaxTree
import qualified Language.Batch.Token as Token

lex :: String -> [Token.Lexeme]
lex code = Lexer.scanLexemes code

parseStage1 :: String -> SyntaxTree.Program
parseStage1 code = Parser.parse code

parseStage2 :: SyntaxTree.Program -> Ast.Program
parseStage2 st = Ast.empty -- TODO

parse :: String -> Ast.Program
parse code = parseStage2 $ parseStage1 code

generateCodeToFile :: Ast.Program -> FilePath -> IO ()
generateCodeToFile = PrettyPrint.printToFile 
