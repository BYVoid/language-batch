{
module Language.Batch.ParserStage2Happy where

import qualified Language.Batch.Ast.Positioned as Ast
import qualified Language.Batch.Lexer as Lexer
import Language.Batch.ParserUtils
import qualified Language.Batch.Token as Token
import Prelude hiding(span)

import Debug.Trace
}

--          parse function    terminal name
%name       expression        expression
%name       setClause         setClause

%tokentype  { Token.Lexeme }
%error      { parseError }

%token
   int          { Token.Lex _ (Token.Int _) }
   param        { Token.Lex _ (Token.Param _) }
   white        { Token.Lex _ (Token.White _) }
   "="          { Token.Lex _ Token.Assign }
   '"'          { Token.Lex _ Token.DoubleQuote }
   "/a"         { Token.Lex _ Token.SlashA }
   "/p"         { Token.Lex _ Token.SlashP }

%nonassoc '"'

%%

setClause
  : setClauseNoSlash { $1 }
  | "/a" white parameter "=" expression {
    Ast.ArithAssign $3 $5 $ span (pos $1) (pos $5)
  }
  | "/a" white '"' parameter "=" expression '"' {
    Ast.ArithAssign $4 $6 $ span (pos $1) (pos $7)
  }
  | "/p" white parameter "=" varstrings {
    Ast.PromptAssign $3 (escape $5) $ span (pos $1) (pos $5)
  }
  | "/p" white '"' parameter "=" varstrings  {
    Ast.PromptAssign $4 (dropLastQuote $6) $ span (pos $1) (pos $6)
  }

setClauseNoSlash
  : parameter {
    Ast.SetDisplay $1 (pos $1)
  }
  | '"' parameter '"' {
    Ast.SetDisplay $2 $ span (pos $1) (pos $3)
  }
  | '"' {
    Ast.SetDisplay (Ast.Identifier "\"" (pos $1)) (pos $1)
  }
  | parameter "=" varstrings {
    Ast.StrAssign $1 (escape $3) $ span (pos $1) (pos $3)
  }
  | '"' parameter "=" varstrings  {
    Ast.StrAssign $2 (dropLastQuote $4) $ span (pos $1) (pos $4)
  }

parameter
  : param {
    Ast.Identifier (exStr $1) (pos $1)
  }

varstring
  : param {
    Ast.String (exStr $1) (pos $1)
  }
  | int {
    Ast.String (show $ exInt $1) (pos $1)
  }
  | white {
    Ast.String (exStr $1) (pos $1)
  }
  | '"' {
    Ast.String "\"" (pos $1)
  }

varstrings
  : { [] }
  | varstring varstrings { $1 : $2 }

expression
  : int {
    Ast.Int (exInt $1) (pos $1)
  }

{
parseExpression :: String -> Ast.Expression
parseExpression code = expression $ Lexer.scanLexemes code

parseSetClause :: String -> Ast.SetClause
parseSetClause code = setClause tokens
  where tokens = Lexer.scanLexemes code
}