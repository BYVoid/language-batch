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

emptyPos = LP 1 1 0 0

startPos :: LexPos -> LexPos -> LexPos
startPos pos startPoint =
  LP {
    lpLine = newLine,
    lpColumn = newColumn,
    lpStartByte = newStartByte,
    lpLength = lpLength pos
  }
  where
    newLine = lpLine pos + lpLine startPoint - 1
    newColumn =
      if lpLine pos == 1 then
        lpColumn pos + lpColumn startPoint - 1
      else
        lpColumn pos
    newStartByte = lpStartByte pos + lpStartByte startPoint

offsetPos :: LexPos -> Int -> LexPos
offsetPos pos offset =
  pos {
    lpColumn = lpColumn pos + offset,
    lpStartByte = lpStartByte pos + offset
  }
