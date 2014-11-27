{-# LANGUAGE FlexibleInstances #-}
module Language.Batch.PrettyPrint where

import Language.Batch.Ast
import qualified Data.ByteString
import Data.ByteString.Lazy(ByteString, putStr, writeFile)
import Data.ByteString.Lazy.Char8(unpack)
import Data.Monoid
import Data.ByteString.Builder(Builder,
                               charUtf8,
                               floatDec,
                               intDec,
                               stringUtf8,
                               toLazyByteString)

class Renderable a where
  render :: Renderable a => a -> Builder

renderSeparateList :: [a] -> String -> (a -> Builder) -> Builder
renderSeparateList list separator renderer = case list of
  [] -> mempty
  [elem] -> renderer elem
  (elem : rest) ->
    build (renderer elem) separator (renderSeparateList rest separator renderer)

instance Renderable Builder where
  render = id

instance Renderable Char where
  render = charUtf8

instance Renderable String where
  render = stringUtf8

instance Renderable Int where
  render = intDec

instance Renderable (PProgram a) where
  render (Program stmts _) = render stmts

instance Renderable (PStatement a) where
  render stmt = case stmt of
    RemComment comment _ -> build "rem " comment
    DoubleColonComment comment _ -> build "::" comment
    Label label _ -> build ":" label
    Goto label _ -> build "goto " label
    Set clause _ -> build "set " clause

instance Renderable [PStatement a] where
  render stmts =
    build (renderSeparateList stmts "\n" render) '\n'

instance Renderable (PSetClause a) where
  render clause = case clause of
    StrAssign ident varstrs _ -> build '"' ident "=" varstrs '"'
    ArithAssign ident expr _ -> build "/a " '"' ident "=" expr '"'
    PromptAssign ident varstrs _ -> build "/p " '"' ident "=" varstrs '"'
    SetDisplay ident _ -> render ident

instance Renderable (PVarString a) where
  render varstr = case varstr of
    Variable var _ -> build '%' var '%'
    String str _ -> render str

instance Renderable [PVarString a] where
  render varstrs =
    renderSeparateList varstrs "" render

instance Renderable (PExpression a) where
  render expr = case expr of
    Int num _ -> render num
    ExprVar var _ -> build '%' var '%'

instance Renderable (PIdentifier a) where
  render (Identifier ident _) = render ident

-- Below is the implementation of varidic parameter of build
class Buildable a where
  bPolyConcat :: [Builder] -> a

instance Buildable Builder where
  bPolyConcat accumulator = mconcat $ reverse accumulator

instance (Buildable a, Renderable b) => Buildable (b -> a) where
  bPolyConcat accumulator = \object ->
    let rendered = render object in
    bPolyConcat (rendered : accumulator)

build :: Buildable a => a
build = bPolyConcat []

generateByteString :: (PProgram a) -> ByteString
generateByteString program = toLazyByteString $ render program

generateString :: (PProgram a) -> String
generateString program = unpack $ generateByteString program

printToStdout :: (PProgram a) -> IO ()
printToStdout program =
  Data.ByteString.Lazy.putStr $ generateByteString program

printToFile :: (PProgram a) -> FilePath -> IO ()
printToFile program filename = Data.ByteString.Lazy.writeFile filename code
  where code = generateByteString program
