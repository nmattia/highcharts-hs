{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}

module Highcharts where

import IHaskell.Display (html)
import Language.JavaScript.JSQQ
import Data.Aeson ((.=), object)
import Highcharts.Chart (Chart, title)
import Highcharts.Chart.Title (text)
import Data.Default (def)
-- import qualified Data.HashMap.Strict as Map
import qualified IHaskell.Display as IHaskell
import qualified Data.Aeson as Aeson
import qualified Data.UUID as UUID
import System.Random (randomIO)

makeDiv :: UUID.UUID -> String
makeDiv uuid = mconcat
    ["<div id=\"", UUID.toString uuid, "\"></div>"]

highchartsConfig :: JS
highchartsConfig = toJS $ object
    [ "paths" .= object
      [ "highcharts" .= "http://code.highcharts.com/6.1.1/highcharts"
      , "highcharts_exports" .=
          "http://code.highcharts.com/6.1.1/modules/exporting"
      ]
    , "shim" .= object
      [ "highcharts" .= object
        [ "exports" .= "Highcharts"
        , "deps" .= ["jquery"]
        ]
      , "highcharts_exports" .= object
        [ "exports" .= "Highcharts"
        , "deps" .= ["highcharts"]
        ]
      ]
    ]

requireStanza :: String
requireStanza = "<script>" <> require <> "</script>"
  where
    require = [js|require.config($_highchartsConfig);|]

highcharts :: UUID.UUID -> Chart -> String
highcharts uuid v = "<script>" <> require <> "</script>"
  where
    require =
      [js|
        require($_exports, function(Highcharts)
          { Highcharts.chart($_uuid, $_v) }
          );
      |]
    exports = ["highcharts_exports"]

-- newtype Chart = Chart Aeson.Value
  -- deriving newtype ToJS

instance IHaskell.IHaskellDisplay Chart where
    display chart = do
        uuid <- randomIO
        pure $
            IHaskell.ManyDisplay $
            (IHaskell.Display . (:[])) <$>
                [ html (makeDiv uuid)
                , html requireStanza
                , html $ highcharts uuid chart
                ]

myChart :: Chart
myChart =
    title (text "foo" def) $
    def
