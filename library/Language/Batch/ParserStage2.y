{
module Language.Batch.ParserStage2 where

import qualified Language.Batch.Ast.Positioned as Ast
import qualified Language.Batch.Lexer as Lexer
import Language.Batch.ParserUtils
import qualified Language.Batch.Token as Token
import Prelude hiding(span)
}

--          parse function    terminal name
%name       expression        expression

%tokentype  { Token.Lexeme }
%error      { parseError }

%token
   int          { Token.Lex _ (Token.Int _) }

%%

expression
  : int {
    Ast.Int (exInt $1) (pos $1)
  }

{
parseExpression :: String -> Ast.Expression
parseExpression code = expression $ Lexer.scanLexemes code
}
