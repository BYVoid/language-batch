module Language.Batch where

import qualified Language.Batch.Ast as Ast
import qualified Language.Batch.Lexer as Lexer
import qualified Language.Batch.Parser as Parser
import qualified Language.Batch.Token as Token

lex :: String -> [Token.Lexeme]
lex code = Lexer.scanLexemes code

parse :: String -> Ast.PProgram ()
parse code = Ast.Program [] ()

generateCodeToFile :: Ast.PProgram () -> FilePath -> IO ()
generateCodeToFile program filename = do return ()
