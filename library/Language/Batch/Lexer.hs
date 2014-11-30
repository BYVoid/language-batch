module Language.Batch.LexerRule where

import Language.Batch.LexerRule

-- Returns scanned lexemes with Right [Token] or Left String on error
scanLexemesSafe :: String -> Either String [Lexeme]
scanLexemesSafe code = runAlex code $ do
  let loop i lexemes = do
      lexeme@(Lex _ token) <- alexMonadScan;
        if token == LEOF then
          return $ reverse lexemes
        else do
          loop (i + 1) (lexeme : lexemes)
  loop 0 []

-- Returns scanned lexemes
scanLexemes :: String -> [Lexeme]
scanLexemes code = case scanLexemesSafe code of
  Left message -> error message
  Right lexemes -> lexemes
