{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad
import Data.Semigroup
import Data.Char (toUpper, isUpper)
import Data.Maybe (mapMaybe)
import System.Directory
import System.Environment
import System.FilePath
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.List as L
import qualified Data.Map.Lazy as Map
import qualified Data.Vector as Vector
-- import Data.Map.Lazy ((!))

main :: IO ()
main = do
    [json, outdir] <- getArgs
    Just v <- Aeson.decode <$> BL8.readFile json
    write (mkFiles v) outdir

mkFiles :: Aeson.Value -> Map.Map FilePath BS.ByteString
mkFiles v = cabal <> moduleContents
  where
    cabal = uncurry Map.singleton (cabalFile modules)
    moduleContents = Map.fromList $
      map
        (\mdl -> do
          let strs = fst mdl
              content = moduleContent mdl
          (L.foldl' (</>) "" strs <> ".hs", content)
        ) (Map.toList modules)
    modules = mkModules ["Highcharts", "Chart"] v

cabalFile :: Modules -> (FilePath, BS.ByteString)
cabalFile modules =
    ("highcharts-types.cabal", BS.intercalate "\n" $
      [ "name: highcharts-types"
      , "version: 0.0.1"
      , "license: MIT"
      , "author: Nicolas Mattia"
      , "maintainer: nicolas@nmattia.com"
      , "copyright: (c) 2018 Nicolas Mattia"
      , "build-type: Simple"
      , "cabal-version: >=1.10"
      , ""
      , "library"
      , "  exposed-modules:"
      ] <> map (\mdl ->
            "    " <> (BS.intercalate "." (BS8.pack <$> mdl))
            ) (Map.keys modules) <>
      [ "  build-depends:"
      , "         base"
      , "       , unordered-containers"
      , "       , js-qq"
      , "       , data-default"
      , "  ghc-options: -Wall -Werror"
      ]
    )


mkModules :: ModuleName -> Aeson.Value -> Modules
mkModules mdlName = \case
    Aeson.Object m -> mkThis m
    v -> error $ "Unexpected value: " <> show v
  where
    mkThis :: Aeson.Object -> Modules
    mkThis m =
      HashMap.foldlWithKey'
        (\ms k v ->
          if k == "_meta" then ms
          else
            let (childType, extraMods) = childInfo (Text.unpack k) v
            in
              Map.insertWith
                (<>)
                mdlName
                (Map.singleton (Text.unpack k) childType)
                ms
              <> extraMods
        ) (Map.singleton mdlName Map.empty) m
    childInfo :: String -> Aeson.Value -> (TypeName, Modules)
    childInfo childName@"title" = \case -- TODO: if has children
      Aeson.Object v -> do
        let
          tyName = upFirst childName
          mdlName' = mdlName <> [tyName]
        (TypeCustom mdlName' tyName,
          mkModules mdlName' (v HashMap.! "children"))
      v -> error $ "unexpected value: " <> show v
    -- childInfo _childName@"text" = \case -- TODO...
      -- Aeson.Object _v -> do
        -- let
          -- ty = TypeString
        -- (ty, Map.empty)
      -- _ -> error "tmp"
    childInfo childName = \case
      Aeson.Object v -> case v HashMap.! "children" of
        Aeson.Object children
          | HashMap.null children ->
              (inferTypeFromDoclet (v HashMap.! "doclet"), Map.empty)
          | otherwise -> do
            let
              tyName = upFirst childName
              mdlName' = mdlName <> [tyName]
            (TypeCustom mdlName' tyName,
              mkModules mdlName' (v HashMap.! "children"))
        children ->
          error $
            "Expected key 'children' to hold an object: " <> show children

      -- (TypeObject, Map.empty)
      v -> error $ "Unexpected child " <> show v
    upFirst (x:xs) = toUpper x:xs
    upFirst xs = xs

inferTypeFromDoclet :: Aeson.Value -> TypeName
inferTypeFromDoclet = \case
    Aeson.Object v -> case HashMap.lookup "type" v of
      Just (Aeson.Object v') -> case HashMap.lookup "names" v' of
        Just (Aeson.Array arr) -> case Vector.head arr of
          "String" -> TypeString
          "string" -> TypeString
          "Number" -> TypeNumber
          "number" -> TypeNumber
          "boolean" -> TypeBool
          "object" -> TypeObject
          _ -> TypeObject
        _v3 -> TypeObject
        -- v3 -> error $ "doclet.type.names is not an array: " <> show v3
      _v2 -> TypeObject
      -- v2 -> error $ "doclet.type is not an object: " <> show v2 <>
              -- " in " <> show v
    v1 -> error $ "doclet is not an object: " <> show v1

-- | returns either a simple type (string, function, etc) or the "children"
-- value.
-- getType :: Aeson.Object -> Either TypeName Aeson.Value
-- getType v = Right (v HashMap.! "children")

type Modules = Map.Map ModuleName Attributes
type AttributeName = String
type Attributes = Map.Map AttributeName TypeName
type Module = (ModuleName, Attributes)

mkModuleName :: ModuleName -> String
mkModuleName = L.intercalate "."

data TypeName
  = TypeString -- A string
  | TypeObject -- A 'JS' object
  | TypeBool -- A boolean
  | TypeNumber -- A boolean
  | TypeCustom ModuleName String

renderTypeName :: TypeName -> String
renderTypeName = \case
    TypeString -> "String"
    TypeBool -> "Bool"
    TypeObject -> "JS"
    TypeNumber -> "Double"
    TypeCustom mdlName tyName -> L.intercalate "." (mdlName <> [tyName])

renderTypeImports :: TypeName -> Maybe ModuleName
renderTypeImports = \case
    TypeString -> Nothing
    TypeObject -> Nothing
    TypeBool -> Nothing
    TypeNumber -> Nothing
    TypeCustom mdlName _ -> Just mdlName

type ModuleName = [String]

moduleContent :: Module -> BS.ByteString
moduleContent (moduleName, moduleAttributes) = BS.intercalate "\n" $
    [ "{-# LANGUAGE DerivingStrategies #-}"
    , "{-# LANGUAGE GeneralizedNewtypeDeriving #-}"
    , ""
    , "module " <> mdlName <> " where"
    , ""
    , "import Language.JavaScript.JSQQ"
    , "import qualified Data.Default as Default"
    , "import qualified Data.HashMap.Strict as Map"
    ] <> (("import qualified " <>) <$> extraImports) <>
    [ ""
    , "newtype " <> tyName <> " = " <> tyName <>
        " { un" <> tyName <> " :: Map.HashMap String JS }"
    , "  deriving newtype ToJS"
    , ""
    , "instance Default.Default " <> tyName <> " where"
    , "  def = " <> tyName <> " Map.empty"
    , ""
    -- TODO: Default instance
    ] <> map (\(attributeName, attributeType) ->
        BS8.pack (unReserve attributeName)  <>
            " :: " <> BS8.pack (renderTypeName attributeType) <>
            " -> " <> tyName <>
            " -> " <> tyName <> "\n" <>
          BS8.pack (unReserve attributeName)  <>
            " v = " <> tyName <> " . Map.insert \"" <>
              BS8.pack attributeName  <> "\" " <>
              "(toJS v) . un" <> tyName <> "\n"
        ) (Map.toList moduleAttributes)
  where
    mdlName = BS8.pack $ mkModuleName moduleName
    tyName = BS8.pack $ L.last moduleName
    unReserve = \case
      "data" -> "data_"
      "type" -> "type_"
      "default" -> "default_"
      x -> if isUpper (head x) then "_" <> x else x
    extraImports =
      fmap BS8.pack $
        fmap mkModuleName $
        mapMaybe renderTypeImports (Map.elems moduleAttributes)

write :: Map.Map FilePath BS.ByteString -> FilePath -> IO ()
write m base = forM_ (Map.toList m) $ \(relPath, content) -> do
    let path = base </> relPath
    createDirectoryIfMissing True (takeDirectory path)
    BS.writeFile path content
