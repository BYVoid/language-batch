module Language.Batch.Ast.Positioned(module Ast,
                                     module Language.Batch.Ast.Positioned) where

import Language.Batch.Ast as Ast
import Language.Batch.Token(LexPos, LexPos(LP))

type Program = PProgram LexPos
type Statement = PStatement LexPos
type SetClause = PSetClause LexPos
type VarString = PVarString LexPos
type Expression = PExpression LexPos
type Identifier = PIdentifier LexPos

empty = Program [] (LP 0 0 0 0)
