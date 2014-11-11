{
module Language.Batch.Lexer where
import Language.Batch.Token
}

%wrapper "monad"

$underscore = \_
$whitechar =  [ \t\n\r\f\v]
$newline =    [\r\n]
$digit =      [0-9]
$oct_digit =  [0-7]
$hex_digit =  [0-9A-Fa-f]
$large =      [A-Z \xc0-\xd6 \xd8-\xde]
$small =      [a-z \xdf-\xf6 \xf8-\xff \_]
$alpha =      [$small $large]

@identifier = [$alpha $underscore] [$alpha $digit $underscore]*

-- Integers
@decimal =    $digit+
@hexadecimal = 0x $hex_digit+
@octal =      0 $oct_digit+

-- Float
@frac =       \. $digit*
@exp =        [eE][\-\+]? $digit+
@float =      $digit* @frac @exp? | $digit* @exp

-- String
$cntrl   = [$large \@\[\\\]\^\_]
@ascii   = \^ $cntrl | NUL | SOH | STX | ETX | EOT | ENQ | ACK
   | BEL | BS | HT | LF | VT | FF | CR | SO | SI | DLE
   | DC1 | DC2 | DC3 | DC4 | NAK | SYN | ETB | CAN | EM
   | SUB | ESC | FS | GS | RS | US | SP | DEL
$charesc = [abfnrtv\\\"\'\&]
@escape  = \\ ($charesc | @ascii | @decimal | @octal | @hexadecimal)
@gap     = \\ $whitechar+ \\
@string_in  = . # [\"\\] | " " | @escape | @gap
@string  = @string_in*

tokens :-
  $white+     { skip }

{

alexEOF :: Alex Lexeme
alexEOF = return (Lex undefined LEOF)

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

}
