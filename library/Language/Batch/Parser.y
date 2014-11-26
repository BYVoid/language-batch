{
module Language.Batch.Parser(parse) where

import qualified Language.Batch.Ast.Positioned as Ast
import qualified Language.Batch.Lexer as Lexer
import Language.Batch.ParserUtils
import qualified Language.Batch.Token as Token
import qualified Language.Batch.SyntaxTree as ST
import Prelude hiding(span)
}

--          parse function    terminal name
%name       program           program
%name       expression        expression

%tokentype  { Token.Lexeme }
%error      { parseError }

%token
   int          { Token.Lex _ (Token.Int _) }
   string       { Token.Lex _ (Token.String _) }
   param        { Token.Lex _ (Token.Param _) }
   rem          { Token.Lex _ (Token.Rem _) }
   doublecolon  { Token.Lex _ (Token.DoubleColon _) }
   assign       { Token.Lex _ (Token.Assign _) }
   set          { Token.Lex _ (Token.Set _) }
   label        { Token.Lex _ Token.Label }
   goto         { Token.Lex _ Token.Goto }
   slash_a      { Token.Lex _ Token.SlashA }
   slash_p      { Token.Lex _ Token.SlashP }
   doublequote  { Token.Lex _ Token.DoubleQuote }
%%

program
  : statements {
    ST.Program $1 $ span (ST.pos $ head $1) (ST.pos $ last $1)
  }

statement
  : rem {
    ST.RemComment (exStr $1) (pos $1)
  }
  | doublecolon {
    ST.DoubleColonComment (exStr $1) (pos $1)
  }
  | label labelname {
    ST.Label $2 $ span (pos $1) (ST.pos $2)
  }
  | goto labelname {
    ST.Goto $2 $ span (pos $1) (ST.pos $2)
  }
  | set {
    ST.Set (unparsed $1) (pos $1)
  }

statements
  : { [] }
  | statement statements { $1 : $2 }

labelname
  : param {
    ST.LabelName (exStr $1) (pos $1)
  }

expression
  : int {
    Ast.Int (exInt $1) (pos $1)
  }

{
parse :: String -> ST.Program
parse code = program $ Lexer.scanLexemes code
}
