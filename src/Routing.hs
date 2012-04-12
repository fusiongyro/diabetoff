{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, OverloadedStrings #-}
module Routing ( module Routing
               , module Web.Routes
               , module Web.Routes.Boomerang
               , module Web.Routes.Happstack) where

import Prelude hiding ((.))
import Control.Category
import Data.Data

import Web.Routes
import Web.Routes.TH
import Web.Routes.Happstack
import Web.Routes.Boomerang
import Text.Boomerang.TH

-- the sitemap

data Sitemap 
  = Home
  | UserPage String
    deriving (Eq, Ord, Read, Show, Data, Typeable)

$(derivePrinterParsers ''Sitemap)

sitemap :: Router Sitemap
sitemap = 
      rHome
  <>  rUserPage . "users" </> anyString
