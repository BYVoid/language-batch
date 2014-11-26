module Language.Batch where

import qualified Language.Batch.Ast.Positioned as Ast
import qualified Language.Batch.Lexer as Lexer
import qualified Language.Batch.ParserStage1 as ParserStage1
import qualified Language.Batch.ParserStage2 as ParserStage2
import qualified Language.Batch.PrettyPrint as PrettyPrint
import qualified Language.Batch.SyntaxTree as SyntaxTree
import qualified Language.Batch.Token as Token

lex :: String -> [Token.Lexeme]
lex code = Lexer.scanLexemes code

parseStage1 :: String -> SyntaxTree.Program
parseStage1 code = ParserStage1.parse code

parseStage2 :: SyntaxTree.Program -> Ast.Program
parseStage2 st = Ast.empty -- TODO

parse :: String -> Ast.Program
parse code = parseStage2 $ parseStage1 code

generateCodeToFile :: Ast.Program -> FilePath -> IO ()
generateCodeToFile = PrettyPrint.printToFile 
