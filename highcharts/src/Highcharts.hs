{-# LANGUAGE QuasiQuotes #-}

module Highcharts where

import IHaskell.Display (html)
import qualified IHaskell.Display as IHaskell
import Data.Aeson as Aeson
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.UUID as UUID
import System.Random (randomIO)

makeDiv :: UUID.UUID -> String
makeDiv uuid = mconcat
    ["<div id=\"", UUID.toString uuid, "\"></div>"]

requireStanza :: String
requireStanza = mconcat
    [ "<script>"
    , "require.config({"
    , "paths: {"
    , "highcharts: \"http://code.highcharts.com/6.1.1/highcharts\","
    , "highcharts_exports: \"http://code.highcharts.com/6.1.1/modules/exporting\","
    , "},"
    , "shim: {"
    , "highcharts: {"
    , "  exports: \"Highcharts\","
    , "  deps: [\"jquery\"]"
    , " },"
    , "highcharts_exports: {"
    , " exports: \"Highcharts\","
    , "  deps: [\"highcharts\"]"
    , "}"
    , "}"
    , "});"
    , "</script>"
    ]

-- | Generate an HTML display.
highcharts :: UUID.UUID -> Aeson.Value -> String
highcharts uuid v = mconcat
    [ "<script>"
    , "require(['highcharts_exports'], function(Highcharts) {"
    , "  Highcharts.chart( '"
          , UUID.toString uuid
          , "',"
          , TL.unpack $ TL.decodeUtf8 $ Aeson.encode v,");"
    , " });"
    , "</script>"
    ]


data Chart = Chart Aeson.Value

instance IHaskell.IHaskellDisplay Chart where
    display (Chart v) = do
        uuid <- randomIO
        pure $
            IHaskell.ManyDisplay $
            (IHaskell.Display . (:[])) <$>
                [ html (makeDiv uuid)
                , html requireStanza
                , html $ highcharts uuid v
                ]
