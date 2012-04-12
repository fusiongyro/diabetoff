module Types ( module Types
             , module Data.Pool) where

import Control.Monad.Reader
import Data.Data

import Happstack.Server
import Data.Pool
import Database.HDBC.PostgreSQL

import Routing

-- some cheesy types

type Name = String
type Weight = Integer
type Password = String

-- my database reader transformer
type DatabasePool = Pool Connection
type DatabaseReader = ReaderT DatabasePool (ServerPartT IO) 
type DiabetoffM a = RouteT Sitemap DatabaseReader a

runDatabaseReader = runReaderT