{-# LANGUAGE MultiParamTypeClasses #-}

module PsetOne.Util where

import           Database.Beam
import           Database.Beam.Backend.SQL
import           Database.Beam.Postgres
import           Database.PostgreSQL.Simple

import           Data.ByteString
import           Data.ByteString.UTF8
import           Data.Time

-- helper for getting database connection
getDbConn :: String -> IO Connection
getDbConn dbName = connect defaultConnectInfo { connectUser     = "ece464"
                                              , connectDatabase = dbName
                                              }

-- helper for running queries with debug printing
runQuery :: IO Connection -> Pg b -> IO b
runQuery conn query =
  conn >>= \conn -> runBeamPostgresDebug Prelude.putStrLn conn query

-- abstracted syntax for joins; easier to use than Beam's `join_`
join
  :: BeamSqlBackend be
  => Q be db s t1                  -- query 1
  -> Q be db s t2                  -- query 2
  -> (t1 -> t2 -> QExpr be s Bool) -- where cond
  -> (t1 -> t2 -> b)               -- field list
  -> Q be db s b                   -- join result
join tab1 tab2 cond res = do
  res1 <- tab1
  res2 <- tab2
  guard_ $ cond res1 res2
  pure $ res res1 res2
