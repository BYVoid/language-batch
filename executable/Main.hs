{-# LANGUAGE Arrows #-}

import Control.Monad
import Options.Applicative
import Options.Applicative.Arrows
import Text.Show.Pretty(ppShow)

import qualified Language.Batch as Batch

data Args = Args FilePath FilePath Opts
  deriving Show

data Opts = Opts
  { optTokens :: Bool,
    optSt :: Bool,
    optAst :: Bool }
  deriving Show

version :: Parser (a -> a)
version = infoOption "0.0.1"
  (  long "version"
  <> short 'v'
  <> help "Print version information" )

parser :: Parser Args
parser = runA $ proc () -> do
  input <- asA (strArgument (metavar "INPUT"
      <> help "Source code file")) -< ()
  output <- asA (strArgument (metavar "TARGET"
      <> help "Target file")) -< ()
  opts <- asA optionParser -< ()
  A version >>> A helper -<  Args input output opts
  where
    optionParser :: Parser Opts
    optionParser = runA $ proc () -> do
      ast <- asA (switch (long "ast"
          <> help "Output parsed abstract syntax tree")) -< ()
      st <- asA (switch (long "st"
          <> help "Output parsed intermediate syntax tree")) -< ()
      tokens <- asA (switch (long "tokens"
          <> help "Output parsed tokens")) -< ()
      returnA -< Opts {
        optAst = ast,
        optSt = st,
        optTokens = tokens}

dispatch :: Args -> IO ()
dispatch (Args input target opts) = do
  code <- readFile input
  let tokens = Batch.lex code
  let syntaxTree = Batch.parseStage1 code
  let ast = Batch.parse code
  let outputWithSuffix :: String -> String -> IO ();
      outputWithSuffix suffix contents = do
        let fileName = target ++ suffix
        writeFile fileName (contents ++ "\n")
  when (optTokens opts) (outputWithSuffix ".tokens" (ppShow tokens))
  when (optSt opts) (outputWithSuffix ".st" (ppShow syntaxTree))
  when (optAst opts) (outputWithSuffix ".ast" (ppShow ast))
  Batch.generateCodeToFile ast target

pinfo :: ParserInfo Args
pinfo = info parser $
  progDesc "Code beautifier for Windows Batch"

main :: IO ()
main = do
  args <- execParser pinfo
  dispatch args
