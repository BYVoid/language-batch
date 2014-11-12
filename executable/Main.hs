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
      tokens <- asA (switch (long "tokens"
          <> help "Output parsed tokens")) -< ()
      returnA -< Opts {
        optAst = ast,
        optTokens = tokens}

dispatch :: Args -> IO ()
dispatch (Args input target opts) = do
  code <- readFile input
  let program = Batch.parse code
  let tokens = Batch.lex code
  let outputWithSuffix :: String -> String -> IO ();
      outputWithSuffix suffix contents = do
        let fileName = target ++ suffix
        writeFile fileName (contents ++ "\n")
  when (optTokens opts) (outputWithSuffix ".tokens" (ppShow tokens))
  when (optAst opts) (outputWithSuffix ".ast" (ppShow program))
  Batch.generateCodeToFile program target

pinfo :: ParserInfo Args
pinfo = info parser $
  progDesc "Code beautifier for Windows Batch"

main :: IO ()
main = do
  args <- execParser pinfo
  dispatch args
