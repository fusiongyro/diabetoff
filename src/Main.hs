{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Control.Monad

--import Paths_diabetoff (version)
import Data.Pool
import Data.Version
import Happstack.Server
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import DiabetoffM
import Pages

version = Version [0,1] []

route :: Sitemap -> DiabetoffM Response
route url =
  case url of
    Home          -> indexPage
    UserPage name -> userPage name

site :: Site Sitemap (DatabaseReader Response)
site = setDefault Home $ boomerangSite (runRouteT route) sitemap

connectionString = "dbname=diabetoff"

setupDatabase' = do
  db <- connect connectionString
  setupDatabase db

main = do
  setupDatabase'
  dbPool <- createPool (connect connectionString) disconnect 1 300 5
  putStrLn $ "Diabetoff v. " ++ showVersion version
  simpleHTTP nullConf $ runDatabaseReader handlers dbPool
    where
      handlers = msum
        [ dir "css"    $ serveDirectory DisableBrowsing [] "css"
        , implSite "" "" site ]