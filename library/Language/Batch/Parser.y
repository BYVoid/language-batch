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
   string   { Token.Lex _ (Token.String _) }

%%

program : string { Program [] dummyPos }

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
exStr (Token.Lex _ (Token.Identifier str)) = str

parseError :: [Token.Lexeme] -> a
parseError _ = error "Parse error"

parse code = program $ Lexer.scanLexemes code

}
