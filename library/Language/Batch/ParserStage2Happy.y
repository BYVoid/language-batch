{
module Language.Batch.ParserStage2Happy where

import qualified Language.Batch.Ast.Positioned as Ast
import qualified Language.Batch.Lexer as Lexer
import Language.Batch.ParserUtils
import qualified Language.Batch.Token as Token
import Prelude hiding(span)
}

--          parse function    terminal name
%name       expression        expression
%name       setClause         setClause

%tokentype  { Token.Lexeme }
%error      { parseError }

%token
   int          { Token.Lex _ (Token.Int _) }
   param        { Token.Lex _ (Token.Param _) }
   assign       { Token.Lex _ (Token.Assign _) }
   "\""         { Token.Lex _ Token.DoubleQuote }
   "/a"         { Token.Lex _ Token.SlashA }
   "/p"         { Token.Lex _ Token.SlashP }
%%

setClause
  : setClauseNoSlash { $1 }

setClauseNoSlash
  : setClauseNoSlashInternal { $1 }
  | "\"" setClauseNoSlashInternal { $2 }

setClauseNoSlashInternal
  : parameter {
    Ast.SetDisplay $1 (pos $1)
  }
  | "\"" {
    Ast.SetDisplay (Ast.Identifier "\"" (pos $1)) (pos $1)
  }
  | parameter assign {
    Ast.StrAssign $1 (parseAssign $ exStr $2) $ span (pos $1) (pos $2)
  }

parameter
  : param {
    Ast.Identifier (exStr $1) (pos $1)
  }

expression
  : int {
    Ast.Int (exInt $1) (pos $1)
  }

{
parseExpression :: String -> Ast.Expression
parseExpression code = expression $ Lexer.scanLexemes code

parseSetClause :: String -> Ast.SetClause
parseSetClause = setClause . Lexer.scanLexemes

parseAssign :: String -> [Ast.VarString]
parseAssign code =
  [Ast.String code dummyPos] -- TODO parse embeded variables
}
