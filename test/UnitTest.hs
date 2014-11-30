import Control.Monad
import Data.Monoid
import qualified Language.Batch as Batch
import qualified Language.Batch.Ast.Positioned as Ast
import qualified Language.Batch.Token as Token
import qualified Language.Batch.SyntaxTree as SyntaxTree
import qualified Language.Batch.PrettyPrint as PrettyPrint
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

main :: IO ()
main = defaultMainWithOpts
  [testCase "Lexer"         testLexer,
   testCase "ParserStage1"  testParser1,
   testCase "ParserStage2"  testParser2,
   testCase "PrettyPrint"   testPrettyPrint]
  mempty

testCaseDir = "test/testcase"
testCases = ["SetCmd"]

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

testParser1 :: Assertion
testParser1 = do
  let testParserFile codeFile astFile = do
      astStr <- readFile astFile
      code <- readFile codeFile
      let expected = read astStr :: SyntaxTree.Program
      let st = Batch.parseStage1 code
      assertEqual (show st) expected st
  forM_ testCases $ \testcase ->
    testParserFile (testCaseDir ++ "/" ++ testcase ++ ".bat")
                   (testCaseDir ++ "/out/" ++ testcase ++ ".bat.syntaxtree")

testParser2 :: Assertion
testParser2 = do
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
