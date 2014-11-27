{-# LANGUAGE FlexibleInstances #-}
module Language.Batch.ParserUtils where

import qualified Language.Batch.Ast.Positioned as Ast
import qualified Language.Batch.Token as Token
import qualified Language.Batch.SyntaxTree as ST
import Prelude hiding(span)

data ParseResult a
  = ParseOk a
  | ParseFail Token.LexPos

thenP :: ParseResult a -> (a -> ParseResult b) -> ParseResult b
thenP monad func = case monad of
  ParseOk res -> func res
  ParseFail pos -> ParseFail pos

returnP :: a -> ParseResult a
returnP res = ParseOk res

parseErrorP :: [Token.Lexeme] -> ParseResult a
parseErrorP [] = error "Unknown parsing error. This could be a bug!"
parseErrorP lexemes =
  ParseFail position
  where
    lexeme = head lexemes
    position = pos lexeme

dummyPos = Token.LP 0 0 0 0

class Posed a where
  pos :: a -> Token.LexPos

-- Get the position information of a Lexeme
instance Posed Token.Lexeme where
  pos (Token.Lex pos _) = pos

-- Get the position information of a AstNode
instance Ast.AstNode a => Posed (a Token.LexPos) where
  pos node = Ast.annot node

instance Posed a => Posed [a] where
  pos nodes = case nodes of
    [] -> Token.LP 0 0 0 0
    _ -> span (pos $ head nodes) (pos $ last nodes)

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
exStr (Token.Lex _ (Token.White str)) = str
exStr (Token.Lex _ (Token.String str)) = str
exStr (Token.Lex _ (Token.Param str)) = str
exStr (Token.Lex _ (Token.Rem str)) = str
exStr (Token.Lex _ (Token.DoubleColon str)) = str
exStr (Token.Lex _ (Token.Set str)) = str
exStr (Token.Lex _ (Token.PercentVar str)) = str

unparsed :: Token.Lexeme -> Int -> ST.Unparsed
unparsed lexeme offset =
  ST.Unparsed (exStr lexeme) (Token.offsetPos (pos lexeme) offset)

displayError :: Token.LexPos -> a
displayError position =
  error $ "Parse error at " ++ (show line) ++ ":" ++ (show column)
  where
    line = Token.lpLine position
    column = Token.lpColumn position

escape :: [Ast.VarString] -> [Ast.VarString]
escape raw = reverse $ foldl process [] raw
  where
    process acc varstr = case acc of
      [] -> [varstr]
      last : rest -> case last of
        Ast.String "^" _ -> doEscape varstr last rest
        _ -> varstr : acc
    doEscape varstr last rest = case varstr of
      Ast.String char _ -> case char of
        "(" -> escaped
        ")" -> escaped
        "\"" -> escaped
        "^" -> escaped
        "&" -> escaped
        "|" -> escaped
        ">" -> escaped
        "<" -> escaped
        _ -> noEscape
      _ -> noEscape
      where
        escaped = varstr : rest
        noEscape = last : rest

dropLastQuote :: [Ast.VarString] -> [Ast.VarString]
dropLastQuote varstrings =
  case varstrings of
    [] -> [] -- TODO warning
    _ -> case lastElement of
      Ast.String "\"" _ -> init varstrings
      _ -> varstrings -- TODO warning
    where lastElement = last varstrings
