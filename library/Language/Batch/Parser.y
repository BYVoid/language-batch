{
module Language.Batch.Parser(parse) where

import Language.Batch.Ast.Positioned as Ast
import qualified Language.Batch.Lexer as Lexer
import qualified Language.Batch.Token as Token
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
   label        { Token.Lex _ Token.Label }
   goto         { Token.Lex _ Token.Goto }
   set          { Token.Lex _ Token.Set }
   slash_a      { Token.Lex _ Token.SlashA }
   slash_p      { Token.Lex _ Token.SlashP }
   doublequote  { Token.Lex _ Token.DoubleQuote }
%%

program
  : statements { Program $1 $ span (pos $ head $1) (pos $ last $1) }

statement
  : rem {
    RemComment (exStr $1) (pos $1)
  }
  | doublecolon {
    DoubleColonComment (exStr $1) (pos $1)
  }
  | label parameter {
    Label $2 $ span (pos $1) (pos $2)
  }
  | goto parameter {
    Goto $2 $ span (pos $1) (pos $2)
  }
  | set_statement {
    Set $1 (pos $1)
  }

set_statement
  : set parameter assign {
    StrAssign $2 (parseAssign $3) $ span (pos $1) (pos $3)
  }
  | set slash_a parameter assign {
    ArithAssign $3 (parseAssignToExpr $4) $ span (pos $1) (pos $4)
  }
  | set slash_p parameter assign {
    PromptAssign $3 (parseAssign $4) $ span (pos $1) (pos $4)
  }
  | set parameter {
    SetDisplay $2 $ span (pos $1) (pos $2)
  }
  | set doublequote {
    SetDisplay (Identifier "\"" (pos $2)) $ span (pos $1) (pos $2)
  }

parameter
  : param {
    Identifier (exStr $1) (pos $1)
  }

statements
  : { [] }
  | statement statements { $1 : $2 }

expression
  : int {
    Int (exInt $1) (pos $1)
  }

{
dummyPos = Token.LP 0 0 0 0

class Posed a where
  pos :: a -> Token.LexPos

-- Get the position information of a Lexeme
instance Posed Token.Lexeme where
  pos (Token.Lex pos _) = pos

-- Get the position information of a AstNode
instance Ast.AstNode a => Posed (a Token.LexPos) where
  pos node = Ast.annot node

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
exStr (Token.Lex _ (Token.Param str)) = str
exStr (Token.Lex _ (Token.Rem str)) = str
exStr (Token.Lex _ (Token.DoubleColon str)) = str

parseAssign :: Token.Lexeme -> [VarString]
parseAssign (Token.Lex pos (Token.Assign str)) =
  [String str pos] -- TODO parse embeded variables

parseAssignToExpr :: Token.Lexeme -> Expression
parseAssignToExpr (Token.Lex pos (Token.Assign str)) =
  expression $ Lexer.scanLexemes str
  -- TODO pos offset

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
