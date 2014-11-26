module Language.Batch.SyntaxTree where

import Language.Batch.Token(LexPos)

data Program
  = Program {
    program_stmts :: [Statement],
    program_pos :: LexPos
  }
  deriving (Eq, Read, Show)

data Statement
  = RemComment {
    stmt_comment :: String,
    stmt_pos :: LexPos
  }
  | DoubleColonComment {
    stmt_comment :: String,
    stmt_pos :: LexPos
  }
  | Label {
    stmt_label :: LabelName,
    stmt_pos :: LexPos
  }
  | Goto {
    stmt_label :: LabelName,
    stmt_pos :: LexPos
  }
  | Set {
    stmt_set :: Unparsed,
    stmt_pos :: LexPos
  }
  deriving (Eq, Read, Show)

data LabelName
  = LabelName {
    label_name :: String,
    label_pos :: LexPos
  }
  deriving (Eq, Read, Show)

data Unparsed
  = Unparsed {
    unparsed_string :: String,
    unparsed_pos :: LexPos
  }
  deriving (Eq, Read, Show)

class SyntaxTreeNode a where
  pos :: a -> LexPos

instance SyntaxTreeNode Program where
  pos = program_pos

instance SyntaxTreeNode Statement where
  pos = stmt_pos

instance SyntaxTreeNode LabelName where
  pos = label_pos

instance SyntaxTreeNode Unparsed where
  pos = unparsed_pos
