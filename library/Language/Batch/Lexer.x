{
module Language.Batch.Lexer where
import Language.Batch.Token
}

%wrapper "monad"

$underscore = \_
$slash =      \/
$whitechar =  [ \t\n\r\f\v]
$newline =    [\r\n]
$digit =      [0-9]
$oct_digit =  [0-7]
$hex_digit =  [0-9A-Fa-f]
$large =      [A-Z \xc0-\xd6 \xd8-\xde]
$small =      [a-z \xdf-\xf6 \xf8-\xff \_]
$alpha =      [$small $large]
$a =          [aA]
$c =          [cC]
$d =          [dD]
$e =          [eE]
$f =          [fF]
$g =          [gG]
$i =          [iI]
$l =          [lL]
$m =          [mM]
$n =          [nN]
$o =          [oO]
$p =          [pP]
$r =          [rR]
$s =          [sS]
$t =          [tT]

@identifier = [$alpha $digit $underscore $slash]+

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
@no_nl = .*

tokens :-
  $white+           { skip }
  $r$e$m" "@no_nl   { makeStringLexeme $ \s -> Rem $ drop 4 s }
  "::"@no_nl        { makeStringLexeme $ \s -> DoubleColon $ drop 2 s }
  "="@no_nl         { makeStringLexeme $ \s -> Assign $ drop 1 s }
  $c$a$l$l          { makeLexeme Call }
  ":"               { makeLexeme Label }
  $g$o$t$o          { makeLexeme Goto }
  $i$f              { makeLexeme If }
  $e$l$s$e          { makeLexeme Else }
  $f$o$r            { makeLexeme For }
  $i$n              { makeLexeme In }
  $d$o              { makeLexeme Do }
  $s$e$t            { makeLexeme Set }
  $s$e$t$l$o$l      { makeLexeme SetLocal }
  "@"               { makeLexeme AtSign }
  "&"               { makeLexeme AndSign }
  "|"               { makeLexeme Pipe }
  "&&"              { makeLexeme And }
  "||"              { makeLexeme Or }
  "("               { makeLexeme LParen }
  ")"               { makeLexeme RParen }
  $slash $a         { makeLexeme SlashA }
  $slash $p         { makeLexeme SlashP }
  @decimal          { makeReadableLexeme Int }
  @identifier       { makeStringLexeme Param }

{
makeLexPos :: AlexPosn -> Int -> LexPos
makeLexPos (AlexPn startByte line column) length =
  LP {lpStartByte = startByte,
      lpLength = length,
      lpLine = line,
      lpColumn = column}

getMatchedString :: AlexInput -> Int -> String
getMatchedString (_, _, _, str) len = take len str

makeStringLexeme :: (String -> Token) -> AlexInput -> Int -> Alex Lexeme
makeStringLexeme cons input len =
  return $ Lex (makeLexPos pos len) token
  where
    (pos, _, _, _) = input
    token = cons $ getMatchedString input len

makeReadableLexeme :: (Read a) =>
                      (a -> Token) -> AlexInput -> Int -> Alex Lexeme
makeReadableLexeme cons input len =
  return $ Lex (makeLexPos pos len) token
  where
    (pos, _, _, _) = input
    token = cons $ read (getMatchedString input len)

makeLexeme :: Token -> AlexInput -> Int -> Alex Lexeme
makeLexeme token (pos, _, _, _) len = return $ Lex (makeLexPos pos len) token

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
