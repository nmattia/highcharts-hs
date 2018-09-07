{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module Highcharts where

import Javascript
import IHaskell.Display (html)
import GHC.TypeLits
import qualified IHaskell.Display as IHaskell
import qualified Data.Aeson as Aeson
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

data ChartArg = ChartArg
  { _series :: Series
  }


data Series
  = SeriesLine Line
  | SeriesBar Bar

data Line = Line
  { allowPointSelect :: Bool
  , name :: String
  , animation :: Animation
  , animationLimit :: Number
  , boostThreshold :: Number
  }

type Number = Float

data Animation = Animation { duration :: Number }

data Bar = Bar
  { name :: String }


-- data Line = Line
  -- { _name :: String

  -- }
