{-# LANGUAGE FlexibleInstances #-}
module Language.Batch.ParserUtils where

import qualified Language.Batch.Ast.Positioned as Ast
import qualified Language.Batch.Token as Token
import qualified Language.Batch.SyntaxTree as ST

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
exStr (Token.Lex _ (Token.Set str)) = str

unparsed :: Token.Lexeme -> ST.Unparsed
unparsed lexeme = ST.Unparsed (exStr lexeme) (pos lexeme)

--parseAssign :: Token.Lexeme -> [VarString]
--parseAssign (Token.Lex pos (Token.Assign str)) =
--  [String str pos] -- TODO parse embeded variables

--parseAssignToExpr :: Token.Lexeme -> Expression
--parseAssignToExpr (Token.Lex pos (Token.Assign str)) =
--  expression $ Lexer.scanLexemes str
--  -- TODO pos offset

parseError :: [Token.Lexeme] -> a
parseError lexemes =
  error $ "Parse error at " ++ (show line) ++ ":" ++ (show column)
  where
    lexeme = head lexemes
    position = pos lexeme
    line = Token.lpLine position
    column = Token.lpColumn position
