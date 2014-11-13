{
module Language.Batch.Parser(parse) where

import Language.Batch.Ast.Positioned as Ast
import qualified Language.Batch.Lexer as Lexer
import qualified Language.Batch.Token as Token
import Prelude hiding(span)
}

%name       program           program

%tokentype  { Token.Lexeme }
%error      { parseError }

%token
   string       { Token.Lex _ (Token.String _) }
   ident        { Token.Lex _ (Token.Identifier _) }
   rem          { Token.Lex _ (Token.Rem _) }
   doublecolon  { Token.Lex _ (Token.DoubleColon _) }
   label        { Token.Lex _ (Token.Label _) }
   goto         { Token.Lex _ Token.Goto }
   set          { Token.Lex _ Token.Set }
%%

program
  : statements { Program [] $ span (apos $ head $1) (apos $ last $1) }

statement
  : rem {
    RemComment (exStr $1) (pos $1)
  }
  | doublecolon {
    DoubleColonComment (exStr $1) (pos $1)
  }
  | label {
    Label (Identifier (exStr $1) (pos $1)) (pos $1)
  }
  | goto identifier {
    Goto $2 $ span (pos $1) (apos $2)
  }

identifier
  : ident {
    Identifier (exStr $1) (pos $1)
  }

statements
  : { [] }
  | statement statements { $1 : $2 }

{
dummyPos = Token.LP 0 0 0 0

-- Get the position information of a Lexeme
pos :: Token.Lexeme -> Token.LexPos
pos (Token.Lex pos _) = pos

-- Get the position information of a AstNode
apos :: Ast.AstNode a => a Token.LexPos -> Token.LexPos
apos node = Ast.annot node

-- Merge two positions from leftmost to rightmost
span :: Token.LexPos -> Token.LexPos -> Token.LexPos
span left right =
  Token.LP
    { Token.lpStartByte = start,
      Token.lpLength = length,
      Token.lpLine = Token.lpLine left,
      Token.lpColumn = Token.lpColumn left}
  where
    length = end - start
    start = Token.lpStartByte left
    end = Token.lpStartByte right + Token.lpLength right

-- Extract internal data of the Lexeme
exInt :: Token.Lexeme -> Int
exInt (Token.Lex _ (Token.Int num)) = num

exStr :: Token.Lexeme -> String
exStr (Token.Lex _ (Token.String str)) = str
exStr (Token.Lex _ (Token.Rem str)) = str
exStr (Token.Lex _ (Token.DoubleColon str)) = str

parseError :: [Token.Lexeme] -> a
parseError lexemes =
  error $ "Parse error at " ++ (show line) ++ ":" ++ (show column)
  where
    lexeme = head lexemes
    position = pos lexeme
    line = Token.lpLine position
    column = Token.lpColumn position

parse :: String -> Program
parse code = program $ Lexer.scanLexemes code

}
