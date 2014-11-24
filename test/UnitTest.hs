import Control.Monad
import Data.Monoid
import qualified Language.Batch as Batch
import qualified Language.Batch.Ast.Positioned as Ast
import qualified Language.Batch.Token as Token
import qualified Language.Batch.PrettyPrint as PrettyPrint
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

main :: IO ()
main = defaultMainWithOpts
  [testCase "Lexer"       testLexer,
   testCase "Parser"      testParser,
   testCase "PrettyPrint" testPrettyPrint]
  mempty

testCaseDir = "test/testcase"
testCases = ["set"]

testLexer :: Assertion
testLexer = do
  let testLexerFile codeFile tokenFile = do
      tokensStr <- readFile tokenFile
      code <- readFile codeFile
      let expected = read tokensStr :: [Token.Lexeme]
      let tokens = Batch.lex code
      assertEqual (show tokens) expected tokens
  forM_ testCases $ \testcase ->
    testLexerFile (testCaseDir ++ "/" ++ testcase ++ ".bat")
                  (testCaseDir ++ "/out/" ++ testcase ++ ".bat.tokens")

testParser :: Assertion
testParser = do
  let testParserFile codeFile astFile = do
      astStr <- readFile astFile
      code <- readFile codeFile
      let expected = read astStr :: Ast.Program
      let ast = Batch.parse code
      assertEqual (show ast) expected ast
  forM_ testCases $ \testcase ->
    testParserFile (testCaseDir ++ "/" ++ testcase ++ ".bat")
                   (testCaseDir ++ "/out/" ++ testcase ++ ".bat.ast")

testPrettyPrint :: Assertion
testPrettyPrint = do
  let testParserFile codeFile prettyFile = do
      prettyStr <- readFile prettyFile
      code <- readFile codeFile
      let expected = prettyStr
      let ast = PrettyPrint.generateString $ Batch.parse code
      assertEqual (show ast) expected ast
  forM_ testCases $ \testcase ->
    testParserFile (testCaseDir ++ "/" ++ testcase ++ ".bat")
                   (testCaseDir ++ "/out/" ++ testcase ++ ".bat")
