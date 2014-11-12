module Language.Batch.Token where

data Token
  = Identifier String
  | String String
  | Int Int
  | Label String
  | Rem String
  | DoubleColon String
  | Call
  | Goto
  | If
  | Else
  | For
  | In
  | Do
  | Set
  | SetLocal
  | AtSign
  | AndSign
  | Pipe
  | And
  | Or
  | LParen
  | RParen
  | LEOF
  deriving (Eq, Read, Show)

data LexPos = LP
  { lpLine :: Int,
    lpColumn :: Int,
    lpStartByte :: Int,
    lpLength :: Int }
  deriving (Eq, Read, Show)

data Lexeme = Lex LexPos Token
  deriving (Eq, Read, Show)
