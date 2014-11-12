module Language.Batch.Ast where

data PProgram annot_type
  = Program {
    program_stmts :: [PStatement annot_type],
    program_annot :: annot_type
  }
  deriving (Eq, Read, Show)

data PStatement annot_type
  = RemComment {
    stmt_comment :: String,
    stmt_annot :: annot_type
  }
  | DoubleColonComment {
    stmt_comment :: String,
    stmt_annot :: annot_type
  }
  | Label {
    stmt_label :: LabelName,
    stmt_annot :: annot_type
  }
  | Goto {
    stmt_label :: LabelName,
    stmt_annot :: annot_type
  }
  | Set {
    stmt_set :: PSetClause annot_type,
    stmt_annot :: annot_type
  }
  deriving (Eq, Read, Show)

data PSetClause annot_type
  -- SET variable=string
  = StrAssign {
    set_ident :: Identifier,
    set_varstrs :: [PVarString annot_type],
    set_annot :: annot_type
  }
  -- SET /A variable=expression
  | ArithAssign {
    set_ident :: Identifier,
    set_expr :: PExpression annot_type,
    set_annot :: annot_type
  }
  -- SET /P variable=[promptString]
  | PromptAssign {
    set_ident :: Identifier,
    set_prompt :: [PVarString annot_type],
    set_annot :: annot_type
  }
  -- SET variable
  -- SET "
  | SetDisplay {
    set_ident :: Identifier,
    set_annot :: annot_type
  }
  deriving (Eq, Read, Show)

data PVarString annot_type
  = Variable {
    varstr_ident :: Identifier,
    varstr_annot :: annot_type
  }
  | String {
    varstr_str :: String,
    varstr_annot :: annot_type
  }
  deriving (Eq, Read, Show)

data PExpression annot_type
  = Int {
    expr_int :: Int,
    expr_annot :: annot_type
  }
  | ExprVar {
    expr_ident :: Identifier,
    expr_annot :: annot_type
  }
  deriving (Eq, Read, Show)

type Identifier = String

type LabelName = String

class AstNode node_type where
  annot :: node_type annot -> annot

instance AstNode PProgram where
  annot = program_annot

instance AstNode PStatement where
  annot = stmt_annot

instance AstNode PSetClause where
  annot = set_annot

instance AstNode PVarString where
  annot = varstr_annot

instance AstNode PExpression where
  annot = expr_annot
