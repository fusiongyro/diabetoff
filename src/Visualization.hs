module Visualization where

import Data.List
import Data.Maybe
import Data.Time.Calendar

import Graphics.GChart
import Graphics.GChart.Types
import Network.URI

import Types
import Database

import Database.HDBC.PostgreSQL

-- | Convert the output of the last week's loss DB function into a Google
-- chart URL
percentWeightLoss :: [(Name, Float)] -> URI
percentWeightLoss dat = fromJust $ parseURI $ escape $ getChartUrl $ do
  setChartSize 400 350
  setChartType Pie
  setChartTitle "Weight Loss"
  setDataEncoding text
  addChartData $ map snd dat
  addColor "224499"
  setLabels $ map fst dat

escape :: String -> String
escape str = escapeURIString (/='|') str

-- | Convert a list of some user's weigh-ins into a Google chart URL
totalWeightLine :: [(Day, Float)] -> URI
totalWeightLine dat = fromJust $ parseURI $ escape $ getChartUrl $ do
  setChartSize 400 350
  setChartType Sparklines
  setChartTitle "Weigh-in Trend"
  setDataEncoding text
  addChartData $ [ point / 300.0 * 100.0 | (_, point) <- dat ]
  addColor "224499"
  addAxis $ makeAxis 
    { axisType = AxisLeft
    , axisRange = Just (Range (0, 300) Nothing) }
  addAxis $ makeAxis 
    { axisType = AxisBottom
    , axisRange = Just (Range (0, toEnum $ length dat) Nothing)
    , axisLabels = Just [show $ fst $ head dat, show $ fst $ last dat ] }

testGenerate :: IO ()
testGenerate = do
  db <- connectPostgreSQL "dbname=diabetoff"
  x <- allWeighIns db "Liz"
  putStrLn $ show $ totalWeightLine x