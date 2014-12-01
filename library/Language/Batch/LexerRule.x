{
module Language.Batch.LexerRule where
import Language.Batch.Token
}

%wrapper "monad"

$underscore = \_
$slash =      \/
$doublequote =\"
$whitechar =  [ \t\f\v]
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

@var = [$alpha $digit $underscore]+
@param = [$alpha $digit $underscore $slash]+

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
  $newline+         { skip }
  $white+           { makeStringLexeme White }
  $r$e$m" "@no_nl   { makeStringLexeme $ \s -> Rem $ drop 4 s }
  "::"@no_nl        { makeStringLexeme $ \s -> DoubleColon $ drop 2 s }
  $s$e$t$white+@no_nl   { makeStringLexeme $ \s -> Set $ drop 4 s }
  $c$a$l$l          { makeLexeme Call }
  ":"               { makeLexeme Label }
  $g$o$t$o          { makeLexeme Goto }
  $i$f              { makeLexeme If }
  $e$l$s$e          { makeLexeme Else }
  $f$o$r            { makeLexeme For }
  $i$n              { makeLexeme In }
  $d$o              { makeLexeme Do }
  $s$e$t$l$o$l      { makeLexeme SetLocal }
  "="               { makeLexeme Assign }
  "@"               { makeLexeme AtSign }
  "&"               { makeLexeme AndSign }
  "|"               { makeLexeme Pipe }
  "&&"              { makeLexeme And }
  "||"              { makeLexeme Or }
  "("               { makeLexeme LParen }
  ")"               { makeLexeme RParen }
  "<"               { makeLexeme Less }
  ">"               { makeLexeme Greater }
  "<="              { makeLexeme LessEqual }
  ">="              { makeLexeme GreaterEqual }
  "^"               { makeLexeme Caret }
  "%"               { makeLexeme Percent }
  "!"               { makeLexeme Exclamation }
  $slash $a         { makeLexeme SlashA }
  $slash $p         { makeLexeme SlashP }
  $doublequote      { makeLexeme DoubleQuote }
  "%"@var"%"        { makeStringLexeme $ \s -> PercentVar $ tail $ init s}
  @decimal          { makeReadableLexeme Int }
  @param            { makeStringLexeme Param }

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
}
