{-# LANGUAGE MultiParamTypeClasses #-}

module Util where

import           Database.Beam
import           Database.Beam.MySQL
import           Database.MySQL.Simple

import           Data.Time

-- not sure why this isn't implemented; allows us to use the `Day` instance
instance FromBackendRow MySQL Day

-- hardcoded for now -- should be okay
dbConnection = connect defaultConnectInfo { connectUser     = "jon"
                                          , connectDatabase = "ece464_pset1"
                                          }

-- helper for running queries with tebug printing
runQuery query =
  dbConnection >>= \conn -> runBeamMySQLDebug putStrLn conn query

-- abstracted syntax for joins; easier to use than Beam's `join_`
join tab1 tab2 cond res = do
  res1 <- tab1
  res2 <- tab2
  guard_ $ cond res1 res2
  pure $ res res1 res2
