{
module Language.Batch.Parser(parse) where

import qualified Language.Batch.Ast as Ast
import qualified Language.Batch.Lexer as Lexer
import qualified Language.Batch.Token as Token
import Prelude hiding(span)
}

%name       program           program

%tokentype  { Token.Lexeme }
%error      { parseError }

%token
   string   { Token.Lex _ (Token.String _) }

%%

program : string { () }

{

parseError :: [Token.Lexeme] -> a
parseError _ = error "Parse error"

parse code = program $ Lexer.scanLexemes code

}
