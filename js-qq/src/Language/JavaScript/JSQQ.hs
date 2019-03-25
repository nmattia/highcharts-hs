{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}

module Language.JavaScript.JSQQ where

import Language.Haskell.TH.Quote
import Data.Maybe
import Control.Monad
import Language.Haskell.TH
import Data.Function
import Data.List
import Data.Text(Text)
import Text.RegexPR
import qualified Data.HashMap.Strict as Map
import qualified Data.UUID as UUID
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL8

newtype JS = JS { unJS :: String } deriving Show

class ToJS a where
  toJS :: a -> JS
  default toJS :: ToJSON a => a -> JS
  toJS = JS . BL8.unpack . encode

instance ToJS Value
instance ToJS Double
instance ToJS Bool
instance ToJS JS where toJS = id
instance ToJS String
instance ToJS Text
instance ToJS UUID.UUID
instance ToJS [String]
instance ToJS [Text]

instance ToJS (Map.HashMap String JS) where
  toJS m = JS $ "{" <> intercalate "," entries <> "}"
      where
        entries =
          Map.elems $ Map.mapWithKey (\k v -> unJS (toJS k) <> ": " <> (unJS v)) m
-- instance ToJSON a => ToJS a where
  -- toJS = JS . BL8.unpack . encode

js :: QuasiQuoter
js = QuasiQuoter
  { quoteExp = replaceJSString
  , quotePat = undefined
  , quoteType = undefined
  , quoteDec = undefined
  }

replaceJSString :: String -> Q Exp
replaceJSString input = do
    replacements <- forM toInterpol $ \(jsident, hsident) ->
      lookupValueName hsident >>= \case
        Nothing -> error $ unwords
          [ "No variable named '" <> hsident <> "' in scope"
          , "when trying to replace", jsident ]
        Just x -> pure (jsident, x)

    replace input replacements
  where
    candidates :: [String]
    candidates = mconcat $ ggetbrsRegexPR "[a-zA-Z_$][0-9a-zA-Z_$]*" input
    toInterpol :: [(String, String)] -- [(JS var name, HS var name)]
    toInterpol = mapMaybe (\x -> (x,) <$> stripPrefix "$_" x) candidates

replace :: String -> [(String, Name)] -> Q Exp
replace input = \case
  (str, name):xs ->
    [|replaceStr str (unJS $ toJS $(pure $ VarE name)) $(replace input xs)|]
  [] -> [|input|]

-- | from -> to -> string -> out
replaceStr :: String -> String -> String -> String
replaceStr from to = fix $ \loop str ->
    case splitAt (length from) str of
      (prefix, rest)
        | prefix == from -> to <> (loop rest)
        | null str -> []
        | otherwise -> head str : loop (tail str)
