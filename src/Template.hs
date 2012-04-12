{-# LANGUAGE OverloadedStrings #-}
module Template where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

baseTemplate :: String -> [H.Html] -> H.Html -> H.Html
baseTemplate title headers body = 
  H.html $ do
    H.head $ do
      H.title $ H.toHtml $ "Diabetoff :: " ++ title
      H.meta ! A.httpEquiv "Content-Type" ! A.content "text/html;charset=utf-8"
      H.link ! A.type_ "text/css" ! A.rel "stylesheet" ! A.href "css/style.css"
      sequence_ headers
    H.body $ do
      body

-- | the essential template: title + body
template :: String -> H.Html -> H.Html
template = (`baseTemplate` [])