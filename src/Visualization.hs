module Visualization where

import Data.List
import Data.Maybe

import Network.URI

import Types

percentWeightLoss :: [(Name, Double)] -> URI
percentWeightLoss dat =
    fromJust $ parseURI $
      concat [ base
             , "?chs=400x350&cht=p&chco=224499&chds=-10,100&chd=t:"
             , dataPoints 
             , "&chdl=" 
             , dataLabels
             , "&chtt=Weight+Loss" ]
  where
    base = "http://chart.apis.google.com/chart"
    dataPoints = escape $ intercalate "," $ map (show . snd) dat
    dataLabels = escape $ intercalate "|" $ map fst dat
    escape str = escapeURIString isUnreserved str
