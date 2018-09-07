{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC "-fno-warn-orphans" #-}

module Language.JavaScript.QQ where

import Data.Data
import Data.Semigroup
import Language.JavaScript.Parser
import Language.JavaScript.Parser.AST
import Language.Haskell.TH.Quote
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

foo :: QuasiQuoter
foo = QuasiQuoter
  { quoteExp = quoteJSString
  , quotePat = undefined
  , quoteType = undefined
  , quoteDec = undefined
  }

foo' :: QuasiQuoter
foo' = QuasiQuoter
  { quoteExp = quoteJSString'
  , quotePat = undefined
  , quoteType = undefined
  , quoteDec = undefined
  }

quoteJSString :: String -> Q Exp
quoteJSString str = case east of
    Left e -> error e
    Right ast -> [|renderToString $(replaceAST ast)|]
  where
    east = parse str "<QuasiQuote>"

quoteJSString' :: String -> Q Exp
quoteJSString' str = case east of
    Left e -> error e
    Right ast -> replaceAST ast
  where
    east = parse str "<QuasiQuote>"

class ToJSExpression a where
  toJSExpression :: JSAnnot -> a -> JSExpression

instance ToJSExpression Int where
  toJSExpression annot x = JSDecimal annot (show x)

instance (Data a, Lift a) => Lift (JSCommaList a)
instance Lift JSAST
instance Lift JSAnnot
instance Lift JSAssignOp
instance Lift JSBinOp
instance Lift JSExpression
instance Lift JSIdent
instance Lift JSSemi
instance Lift JSStatement

-- replaceJSStatement :: JSStatement -> Q JSStatement
-- replaceJSStatement = \case
  -- a -> pure a

replaceAST :: JSAST -> Q Exp
replaceAST = \case
  JSAstExpression expr annot ->
    [|JSAstExpression $(replaceJSExpression expr) annot|]
  JSAstLiteral expr annot ->
    [|JSAstLiteral $(replaceJSExpression expr) annot|]
  JSAstProgram sts annot ->
    [|JSAstProgram $(replaceJSStatements sts) annot|]
  JSAstStatement st annot ->
    [|JSAstProgram st annot|]

replaceJSStatements :: [JSStatement] -> Q Exp
replaceJSStatements = \case
  (x:xs) -> [|$(replaceJSStatement x) : $(replaceJSStatements xs)|]
  [] -> [|[]|]

replaceJSStatement :: JSStatement -> Q Exp
replaceJSStatement = \case
  JSExpressionStatement expr semi ->
    [|JSExpressionStatement $(replaceJSExpression expr) semi|]
  JSAssignStatement expr op expr' sem ->
    [|JSAssignStatement expr op $(replaceJSExpression expr') sem|]
  JSStatementBlock annot sts annot' sem ->
    [|JSStatementBlock annot $(replaceJSStatements sts) annot' sem|]
  JSBreak annot ident sem ->
    [|JSBreak annot ident sem|]
  JSConstant annot xs sem ->
    [|JSConstant annot $(replaceJSCommaList xs) sem|]
  JSContinue annot ident sem ->
    [|JSContinue annot ident sem|]
  JSDoWhile annot1 st annot2 annot3 expr annot4 sem ->
    [|JSDoWhile annot1 $(replaceJSStatement st) annot2 annot3
        $(replaceJSExpression expr) annot4 sem
    |]
  JSFor annot1 annot2 xs1 annot3 xs2 annot4 xs3 annot5 st ->
    [|JSFor annot1 annot2 $(replaceJSCommaList xs1) annot3
        $(replaceJSCommaList xs2) annot4 $(replaceJSCommaList xs3) annot5 st
    |]
  JSForIn annot1 annot2 expr1 op expr2 annot3 st ->
    [|JSForIn annot1 annot2 $(replaceJSExpression expr1) op
        $(replaceJSExpression expr2) annot3 st
    |]
  JSForVar annot1 annot2 annot3 xs1 annot4 xs2 annot5 xs3 annot6 st ->
    [|JSForVar annot1 annot2 annot3 $(replaceJSCommaList xs1) annot4
        $(replaceJSCommaList xs2) annot5 $(replaceJSCommaList xs3) annot6
        $(replaceJSStatement st)
    |]
  JSForVarIn annot1 annot2 annot3 expr1 op expr2 annot4 st ->
    [|JSForVarIn annot1 annot2 annot3 $(replaceJSExpression expr1) op
        $(replaceJSExpression expr2) annot4 $(replaceJSStatement st)
    |]
  JSFunction annot1 ident annot2 xs annot3 block sem ->
    [|JSFunction annot1 ident annot2 xs annot3
        $(replaceJSBlock block) sem
    |]
  JSIf annot1 annot2 expr annot3 st ->
    [|JSIf annot1 annot2 $(replaceJSExpression expr) annot3
        $(replaceJSStatement st)
    |]
  JSLabelled ident annot st ->
    [|JSLabelled ident annot $(replaceJSStatement st)|]

replaceJSBlock :: JSBlock -> Q Exp
replaceJSBlock (JSBlock annot1 sts annot2) =
    [|JSBlock annot1 $(replaceJSStatements sts) annot2|]

replaceJSCommaList :: JSCommaList JSExpression -> Q Exp
replaceJSCommaList = \case
    JSLCons xs annot a ->
      [|JSLCons $(replaceJSCommaList xs) annot $(replaceJSExpression a)|]
    JSLOne a ->
      [|JSLOne $(replaceJSExpression a)|]
    JSLNil -> [|JSLNil|]

replaceJSExpression :: JSExpression -> Q Exp
replaceJSExpression = \case
  JSIdentifier annot ('$' : '_' : ident) -> do
      x <- lookupValueName ident >>= \case
        Nothing -> error $ "No variable named '" <> ident <> "' in scope"
        Just x -> pure x
      [|toJSExpression annot $(pure $ VarE x)|]
  JSAssignExpression lhs op e ->
    [|JSAssignExpression lhs op $(replaceJSExpression e)|]
  JSExpressionBinary expr op expr' ->
    [|JSExpressionBinary $(replaceJSExpression expr) op
      $(replaceJSExpression expr')|]
  a -> [|a|]

getJSExpressionIdentifier :: JSExpression -> Maybe String
getJSExpressionIdentifier = \case
  JSIdentifier _ ('$':'_':identifier) -> Just identifier
  _ -> Nothing
