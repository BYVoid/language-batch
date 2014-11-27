{
module Language.Batch.ParserStage1(parse) where

import qualified Language.Batch.Lexer as Lexer
import Language.Batch.ParserUtils
import qualified Language.Batch.Token as Token
import qualified Language.Batch.SyntaxTree as ST
import Prelude hiding(span)
}

--          parse function    terminal name
%name       program           program

%tokentype  { Token.Lexeme }
%monad      { ParseResult } { thenP } { returnP }
%error      { parseErrorP }

%token
   string       { Token.Lex _ (Token.String _) }
   param        { Token.Lex _ (Token.Param _) }
   rem          { Token.Lex _ (Token.Rem _) }
   doublecolon  { Token.Lex _ (Token.DoubleColon _) }
   set          { Token.Lex _ (Token.Set _) }
   label        { Token.Lex _ Token.Label }
   goto         { Token.Lex _ Token.Goto }
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

{
parse :: String -> ST.Program
parse code = case program tokens of
  ParseOk result -> result
  ParseFail pos -> displayError pos
  where tokens = Lexer.scanLexemes code
}
