module Language.Batch.Token where

data Token
  = White String
  | Param String
  | String String
  | Int Int
  | PercentVar String
  | Rem String
  | DoubleColon String
  | Set String
  | Call
  | Label
  | Goto
  | If
  | Else
  | For
  | In
  | Do
  | SetLocal
  | Assign
  | AtSign
  | AndSign
  | Pipe
  | And
  | Or
  | LParen
  | RParen
  | SlashA
  | SlashP
  | DoubleQuote
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

emptyPos = LP 0 0 0 0
