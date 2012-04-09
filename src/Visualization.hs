module Visualization where

import Data.List
import Data.Maybe

import Graphics.GChart
import Graphics.GChart.Types
import Network.URI

import Types

percentWeightLoss :: [(Name, Float)] -> URI
percentWeightLoss dat =
    fromJust $ parseURI $ escape $ getChartUrl $ do
      setChartSize 400 350
      setChartType Pie
      setChartTitle "Weight Loss"
      setDataEncoding text
      addChartData $ map snd dat
      addColor "224499"
      setLabels $ map fst dat
    where
      escape str = escapeURIString (/='|') str