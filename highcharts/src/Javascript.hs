{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Javascript where

import Language.JavaScript.Parser
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

quoteJSString :: String -> Q Exp
quoteJSString str = case east of
    Left e -> error e
    Right ast -> replaceAST ast
    -- Right ast -> [|renderToString $(replaceAST ast)|]
      -- ast' <- replaceAST ast
      -- let out = renderToString ast'
      -- [|out|]
  where
    east = parse str "<QuasiQuote>"

class ToJSExpression a where
  toJSExpression :: a -> JSExpression

instance ToJSExpression Int where
  toJSExpression x = JSDecimal JSNoAnnot (show x)

instance Lift JSExpression
instance Lift JSAST
instance Lift JSAnnot
instance Lift JSAssignOp
instance Lift JSStatement
instance Lift JSSemi

-- replaceJSStatement :: JSStatement -> Q JSStatement
-- replaceJSStatement = \case
  -- a -> pure a

replaceAST :: JSAST -> Q Exp
replaceAST = \case
  JSAstExpression exp annot ->
    [|JSAstExpression $(replaceJSExpression exp) annot|]
  JSAstLiteral exp annot ->
    [|JSAstLiteral $(replaceJSExpression exp) annot|]
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
  JSExpressionStatement exp semi ->
    [|JSExpressionStatement $(replaceJSExpression exp) semi|]
  JSAssignStatement exp op exp' sem ->
    [|JSAssignStatement exp op $(replaceJSExpression exp') sem|]

replaceJSExpression :: JSExpression -> Q Exp
replaceJSExpression = \case
  JSIdentifier annot foo -> do
      Just x <- lookupValueName foo
      [|toJSExpression $(pure $ VarE x)|]
  or@(JSAssignExpression lhs op e) -> case getJSExpressionIdentifier e of
    Just str -> do
      Just x <- lookupValueName str
      [|JSAssignExpression lhs op $(pure $ VarE x)|]
    Nothing ->
      [|JSAssignExpression lhs op $(replaceJSExpression e)|]
  a -> [|a|]

getJSExpressionIdentifier :: JSExpression -> Maybe String
getJSExpressionIdentifier = \case
  JSIdentifier _ identifier -> Just identifier
  _ -> Nothing
