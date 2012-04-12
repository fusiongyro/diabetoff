module DiabetoffM ( module DiabetoffM
                  , module Types
                  , module Database
                  , module Routing) where

import Control.Monad.Reader
import Control.Monad

import Types
import Database
import Routing

withDatabase :: (Connection -> IO b) -> DiabetoffM b
withDatabase ma = do
  pool <- ask
  liftIO $ withResource pool ma
