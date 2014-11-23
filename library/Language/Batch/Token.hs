module Language.Batch.Token where

data Token
  = Param String
  | String String
  | Int Int
  | Rem String
  | DoubleColon String
  | Assign String
  | Call
  | Label
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
  | SlashA
  | SlashP
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
