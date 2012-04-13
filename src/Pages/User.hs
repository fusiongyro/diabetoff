{-# LANGUAGE OverloadedStrings #-}
module Pages.User where

import Happstack.Server
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

import DiabetoffM
import Visualization
import Template

userPage :: Name -> DiabetoffM Response
userPage name = do
  weighIns <- withDatabase (`allWeighIns` name)
  let chartUrl = totalWeightLine weighIns
  ok $ toResponse $ template name $ do
    H.h2 $ toHtml name
    H.img ! A.src (toValue $ show chartUrl)
