{-# LANGUAGE OverloadedStrings #-}
module IndexPage where

import Happstack.Server
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

import DiabetoffM
import Visualization
import Template

indexPage :: DiabetoffM Response
indexPage = do
  loss <- withDatabase lastWeeksLoss
  let chartUrl = percentWeightLoss loss
  ok $ toResponse $ template "Loss Leaders" $ do
    H.h2 $ "Loss Leaders"
    H.img ! A.src (toValue $ show chartUrl)