{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Language.Batch.ParserStage2(parse) where

import Debug.Trace
import qualified Language.Batch.Ast.Positioned as Ast
import qualified Language.Batch.Lexer as Lexer
import Language.Batch.ParserStage2Happy
import Language.Batch.ParserUtils
import qualified Language.Batch.SyntaxTree as ST
import qualified Language.Batch.Token as Token

parse :: ST.Program -> Ast.Program
parse program = convert program

class Convertable a b where
  convert :: a -> b

instance Convertable ST.Program Ast.Program where
  convert (ST.Program stmts pos) = Ast.Program (convert stmts) pos

instance Convertable [ST.Statement] [Ast.Statement] where
  convert stmts = map convert stmts

instance Convertable ST.Statement Ast.Statement where
  convert stmt = case stmt of
    ST.RemComment comment pos -> Ast.RemComment comment pos
    ST.DoubleColonComment comment pos -> Ast.DoubleColonComment comment pos
    ST.Label label pos -> Ast.Label (convert label) pos
    ST.Goto label pos -> Ast.Goto (convert label) pos
    ST.Set clause pos -> Ast.Set (convert clause) pos

instance Convertable ST.LabelName Ast.Identifier where
  convert (ST.LabelName label pos) = Ast.Identifier label pos

instance Convertable ST.Unparsed Ast.SetClause where
  convert (ST.Unparsed code pos) = parseSetClause code pos

parseSetClause :: String -> Token.LexPos -> Ast.SetClause
parseSetClause code startPos = case setClause tokens of
  ParseOk result -> result
  ParseFail pos -> displayError $ Token.startPos pos startPos
  where tokens = Lexer.scanLexemes code
