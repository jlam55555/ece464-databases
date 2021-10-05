{-# LANGUAGE MultiParamTypeClasses #-}

module Util where

import           Database.Beam
import           Database.Beam.Postgres
import           Database.PostgreSQL.Simple

import           Data.Time

-- hardcoded for now -- should be okay
dbConnection = connect defaultConnectInfo { connectUser     = "ece464"
                                          , connectDatabase = "ece464_pset1"
                                          }

-- helper for running queries with tebug printing
runQuery query =
  dbConnection >>= \conn -> runBeamPostgresDebug putStrLn conn query

-- abstracted syntax for joins; easier to use than Beam's `join_`
join tab1 tab2 cond res = do
  res1 <- tab1
  res2 <- tab2
  guard_ $ cond res1 res2
  pure $ res res1 res2
