{
module Language.Batch.ParserStage2Happy where

import qualified Language.Batch.Ast.Positioned as Ast
import Language.Batch.ParserUtils
import qualified Language.Batch.Token as Token
import Prelude hiding(span)

}

--          parse function    terminal name
%name       expression        expression
%name       setClause         setClause

%tokentype  { Token.Lexeme }
%monad      { ParseResult } { thenP } { returnP }
%error      { parseErrorP }

%token
   int          { Token.Lex _ (Token.Int _) }
   param        { Token.Lex _ (Token.Param _) }
   percent_var  { Token.Lex _ (Token.PercentVar _) }
   white        { Token.Lex _ (Token.White _) }
   "="          { Token.Lex _ Token.Assign }
   '"'          { Token.Lex _ Token.DoubleQuote }
   '&'          { Token.Lex _ Token.AndSign }
   '|'          { Token.Lex _ Token.Pipe }
   '('          { Token.Lex _ Token.LParen }
   ')'          { Token.Lex _ Token.RParen }
   '^'          { Token.Lex _ Token.Caret }
   '<'          { Token.Lex _ Token.Less }
   '>'          { Token.Lex _ Token.Greater }
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
  | '&' {
    Ast.String "&" (pos $1)
  }
  | '|' {
    Ast.String "|" (pos $1)
  }
  | '(' {
    Ast.String "(" (pos $1)
  }
  | ')' {
    Ast.String ")" (pos $1)
  }
  | '^' {
    Ast.String "^" (pos $1)
  }
  | '<' {
    Ast.String "<" (pos $1)
  }
  | '>' {
    Ast.String ">" (pos $1)
  }
  | var {
    Ast.Variable $1 (pos $1)
  }

varstrings
  : { [] }
  | varstring varstrings { $1 : $2 }

var
  : percent_var {
    Ast.Identifier (exStr $1) (pos $1)
  } 

expression
  : int {
    Ast.Int (exInt $1) (pos $1)
  }
