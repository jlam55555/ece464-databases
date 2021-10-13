{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module PsetOne.Util where

import           Database.Beam
import           Database.Beam.Backend.SQL
import           Database.Beam.Postgres
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.Types

import           GHC.Base                       ( liftM
                                                , sequence
                                                )

import           Data.ByteString.UTF8           ( fromString )
import           Data.Int                       ( Int64 )
import           Data.Time

-- helper to print out each item on new line;
-- query is some query that returns an (IO item) type
showLines :: (Show a) => IO [a] -> IO ()
showLines = flip (>>=) (\list -> mapM_ (putStrLn . show) list)

-- helper functions for time literals
makeDay = fromGregorian
makeTime yyyy mM dd hh mm ss =
  LocalTime (fromGregorian yyyy mM dd) (TimeOfDay hh mm ss)

-- helper for running a SQL file
runSqlFile :: IO Connection -> FilePath -> IO Int64
runSqlFile conn file = do
  text <- readFile file
  conn <- conn
  execute_ conn $ Query $ fromString text

-- helper for getting database connection
getDbConn :: String -> IO Connection
getDbConn dbName = connect defaultConnectInfo { connectUser     = "ece464"
                                              , connectDatabase = dbName
                                              }

-- helper for running queries with debug printing
runQuery :: IO Connection -> Pg b -> IO b
runQuery conn query =
  conn >>= \conn -> runBeamPostgresDebug putStrLn conn query

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

-- note: beam doesn't have a field selector option, but it is basically
-- a monad bind operator (much like `showLines`), so this is omittted
-- from_ query selector = do res <- query; pure $ selector res
-- from_ query selector = query >>= \res -> pure selector res
-- from_ query selector = query >>= selector
-- from_ = (>>=)

-- alternatively: selector `from_` query
-- from_ = (=<<)
